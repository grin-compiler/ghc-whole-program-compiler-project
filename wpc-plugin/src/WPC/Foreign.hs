module WPC.Foreign where

import           Control.Monad                 (Functor (..), forM)
import           System.IO                     (putStrLn)

import           Data.Bool                     (Bool (..), otherwise)
import           Data.Function                 (($), (.))
import           Data.IORef                    (modifyIORef)
import           Data.List                     (concat, concatMap, map, unzip3, (++))
import           Data.Maybe                    (Maybe (..), mapMaybe)
import           Data.Monoid                   (Monoid (..))
import           Data.String                   (String)
import           Data.Tuple                    (snd)

import           GHC.Core.TyCo.Rep             (Scaled (..))
import           GHC.Data.OrdList              (OrdList, fromOL)
import           GHC.Driver.Hooks              (Hooks (..))
import           GHC.Hs.Extension              (GhcTc)
import           GHC.HsToCore.Foreign.Decl     (dsForeigns)
import           GHC.HsToCore.Types            (DsM)
import           GHC.Plugins                   (Alt (..), Bind (..), Coercion, CoreAlt, CoreBind, CoreExpr, Expr (..),
                                                FastString, FunctionOrData (..), GenLocated (..), HscEnv (..), Id,
                                                Literal (..), NamedThing (..), Outputable (..), TyCon, Type,
                                                anonPiTyBinderType_maybe, coercionLKind, getOccString, liftIO, pprPanic,
                                                tcSplitTyConApp_maybe, unLoc)
import           GHC.Tc.Utils.Monad            (Applicative (..), updTopEnv)
import           GHC.Tc.Utils.TcType           (tcSplitForAllInvisTyVars, tcSplitFunTys, tcSplitIOType_maybe,
                                                tcSplitPiTys)
import           GHC.Types.ForeignStubs        (ForeignStubs)
import           GHC.Types.RepType             (unwrapType)
import           GHC.Err                       (undefined)

import           Language.Haskell.Syntax.Decls (CImportSpec (..), ForeignDecl (..), ForeignImport (..), LForeignDecl)

import           WPC.ForeignStubDecls          (StubDecl (..), StubImpl (..), mergeForeignStubs)
import           WPC.GlobalEnv                 (GlobalEnv (..), globalEnvIORef)

dsForeignsFun :: [LForeignDecl GhcTc] -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
dsForeignsFun fos = do
  liftIO $ putStrLn " ###### dsForeignsFun"
  updTopEnv (\hscEnv -> hscEnv {hsc_hooks = (hsc_hooks hscEnv) {dsForeignsHook = Nothing}}) $ do

    resultList <- forM fos $ \fo -> do
      (stubs, bindings) <- dsForeigns [fo]
      let stubDecl = mkStubDecl stubs bindings fo
      pure (stubs, bindings, stubDecl)

    let (stubList, bindingList, stubDeclList) = unzip3 resultList
    liftIO $ modifyIORef globalEnvIORef $ \d -> d
      { geStubDecls = Just stubDeclList
      }

    pure (mergeForeignStubs stubList, mconcat bindingList)

mkStubDecl :: ForeignStubs -> OrdList (Id, CoreExpr) -> LForeignDecl GhcTc -> (ForeignStubs, StubDecl)
mkStubDecl stub bindings (L _loc decl) = case decl of
  ForeignImport{..} -> (stub, StubDeclImport fd_fi (mkStubImpl bindings decl))
  ForeignExport{..} -> (stub, StubDeclExport fd_fe (unLoc fd_name))

mkStubImpl :: OrdList (Id, CoreExpr) -> ForeignDecl GhcTc -> Maybe StubImpl
mkStubImpl bindings decl = case decl of
  ForeignImport{..}
    | CImport _srcText _cconv _safety _mHeader CWrapper <- fd_fi
    , [wrapperCName] <- concat $ (map (getWrapperName . snd) $ fromOL bindings)
    , (isIOCall, retTy, argTys) <- getCWrapperDescriptor fd_i_ext
    -> Just $ StubImplImportCWrapper
        { siCWrapperLabel   = wrapperCName
        , siStdCallArgSize  = Nothing
        , siIsIOCall        = isIOCall
        , siReturnType      = retTy
        , siArgTypes        = argTys
        }

  _ -> Nothing
 where
  goBind :: CoreBind -> [FastString]
  goBind = \case
    NonRec _ e  -> getWrapperName e
    Rec l       -> concatMap (getWrapperName . snd) l

  goAlt :: CoreAlt -> [FastString]
  goAlt (Alt _ _ e) = getWrapperName e

  getWrapperName :: CoreExpr -> [FastString]
  getWrapperName expr = case expr of
    App e a                                     -> getWrapperName e ++ getWrapperName a
    Lam _ e                                     -> getWrapperName e
    Let b e                                     -> goBind b ++ getWrapperName e
    Case e _ _ l                                -> getWrapperName e ++ concatMap goAlt l
    Cast e _                                    -> getWrapperName e
    Tick _ e                                    -> getWrapperName e

    Var{}                                       -> []
    Lit (LitLabel fe_nm _mb_sz_args IsFunction) -> [fe_nm]
    Lit{}                                       -> []
    Type{}                                      -> []
    Coercion{}                                  -> []

getCWrapperDescriptor :: Coercion -> (Bool, String, [String]) -- is IO, result type, arg types
getCWrapperDescriptor ffiCo = (is_IO_res_ty, showFFIType res_ty, fmap showFFIType fe_arg_tys)
  where
    -- example for ffiTy: (Int -> IO Int) -> IO (FunPtr (Int -> IO Int))
    ffiTy                   = coercionLKind ffiCo
    (_,sans_foralls)        = tcSplitForAllInvisTyVars ffiTy
    -- example for arg_ty: Int -> IO Int
    Scaled _ arg_ty = case tcSplitFunTys sans_foralls of
      ([], _) -> undefined
      (a : _, _) -> a


    (bndrs, orig_res_ty)   = tcSplitPiTys arg_ty
    fe_arg_tys             = mapMaybe anonPiTyBinderType_maybe bndrs

    -- Look at the result type of the exported function, orig_res_ty
    -- If it's IO t, return         (t, True)
    -- If it's plain t, return      (t, False)
    (res_ty, is_IO_res_ty) = case tcSplitIOType_maybe orig_res_ty of
                             -- The function already returns IO t
                             Just (_ioTyCon, res_ty') -> (res_ty', True)
                             -- The function returns t
                             Nothing                  -> (orig_res_ty, False)

    showFFIType :: Type -> String
    showFFIType t = getOccString (getName (typeTyCon t))

    typeTyCon :: Type -> TyCon
    typeTyCon ty
      | Just (tc, _) <- tcSplitTyConApp_maybe (unwrapType ty)
      = tc
      | otherwise
      = pprPanic "GHC.HsToCore.Foreign.C.typeTyCon" (ppr ty)
