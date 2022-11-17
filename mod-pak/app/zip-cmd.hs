import Control.Monad.IO.Class
import Options.Applicative
import qualified Data.ByteString as BS
import System.FilePath
import Codec.Archive.Zip
import Codec.Archive.Zip.Unix

data ZipCommand
  = CreateArchive   FilePath
  | WithArchive     FilePath
  | DoesEntryExist  FilePath
  | GetEntry        FilePath
  | SaveEntry       FilePath FilePath
  | CheckEntry      FilePath
  | UnpackInto      FilePath
  | GetArchiveComment
  | GetArchiveDescription
  | AddEntry        BS.ByteString FilePath
  | LoadEntry       FilePath FilePath
  | CopyEntry       FilePath FilePath FilePath
  deriving Show

zipCommand :: Parser ZipCommand
zipCommand = hsubparser $ mconcat
  [ command "CreateArchive"
    (info
      (CreateArchive <$> argument str (metavar "FILENAME"))
      (progDesc "Create a new archive given its location. This will silently overwrite the specified file if it already exists.")
    )
  , command "WithArchive"
    (info
      (WithArchive <$> argument str (metavar "FILENAME"))
      (progDesc "Work with an existing archive.")
    )
  , command "DoesEntryExist"
    (info
      (DoesEntryExist <$> argument str (metavar "FILENAME"))
      (progDesc "Check whether the specified entry exists in the archive.")
    )
  , command "GetEntry"
    (info
      (GetEntry <$> argument str (metavar "FILENAME"))
      (progDesc "Get contents of a specific archive entry as a strict ByteString. It's not recommended to use this on big entries, because it will suck out a lot of memory.")
    )
  , command "SaveEntry"
    (info
      (SaveEntry <$> argument str (metavar "FILENAME") <*> argument str (metavar "TARGET"))
      (progDesc "Save a specific archive entry as a file in the file system.")
    )
  , command "CheckEntry"
    (info
      (CheckEntry <$> argument str (metavar "FILENAME"))
      (progDesc "Calculate CRC32 check sum and compare it with the value read from the archive. The function returns True when the check sums are the same—that is, the data is not corrupted.")
    )
  , command "UnpackInto"
    (info
      (UnpackInto <$> argument str (metavar "FILENAME"))
      (progDesc "Unpack the archive into the specified directory. The directory will be created if it does not exist.")
    )
  , command "GetArchiveComment"
    (info
      (pure GetArchiveComment)
      (progDesc "Get the archive comment.")
    )
  , command "GetArchiveDescription"
    (info
      (pure GetArchiveDescription)
      (progDesc "Get the archive description record.")
    )
  , command "AddEntry"
    -- TODO: add compression method
    (info
      (AddEntry <$> argument str (metavar "BYTESTRING") <*> argument str (metavar "FILENAME"))
      (progDesc "Get the archive description record.")
    )
  , command "LoadEntry"
    -- TODO: add compression method
    (info
      (LoadEntry <$> argument str (metavar "FILENAME") <*> argument str (metavar "TARGET"))
      (progDesc "Load an entry from a given file.")
    )
  , command "CopyEntry"
    (info
      (CopyEntry <$> argument str (metavar "FILENAME") <*> argument str (metavar "SOURCE") <*> argument str (metavar "TARGET"))
      (progDesc "Copy an entry “as is” from another zip archive.")
    )
  ]
{-
  zip file specification:
    createArchive
    withArchive

  commands:
    doesEntryExist
    getEntry
    saveEntry
    checkEntry
    unpackInto
    getArchiveComment
    getArchiveDescription
    addEntry
    loadEntry
    copyEntry

    packDirRecur
    renameEntry
    deleteEntry
    recompress
    setEntryComment
    deleteEntryComment
    setModTime
    addExtraField
    deleteExtraField
    setExternalFileAttrs
    setArchiveComment
    deleteArchiveComment

TODO:
  - use named options instead of arguments
  - add RunZipCommandsFromFile
  - add field names to command adt constructors
  - support regex patterns for filtering (i.e. unpack only the selected content)
-}


main :: IO ()
main = do
  let opts = info (many zipCommand <**> helper) fullDesc
  cmds <- execParser opts
  print cmds
  pure ()
