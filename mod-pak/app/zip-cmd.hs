import Control.Monad.IO.Class
import Options.Applicative
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.FilePath
import Codec.Archive.Zip
import Codec.Archive.Zip.Unix

data ZipCommand
  = CreateArchive
    { zipPath :: FilePath
    }
  | WithArchive
    { zipPath :: FilePath
    }
  | DoesEntryExist
    { entryPath :: FilePath
    }
  | GetEntry
    { entryPath :: FilePath
    }
  | SaveEntry
    { entryPath   :: FilePath
    , targetPath  :: FilePath
    }
  | CheckEntry
    { entryPath :: FilePath
    }
  | UnpackInto
    { targetPath :: FilePath
    }
  | GetArchiveComment
  | GetArchiveDescription
  | AddEntry
    { compressionMethod :: CompressionMethod
    , entryContent      :: ByteString
    , entryPath         :: FilePath
    }
  | LoadEntry
    { compressionMethod :: CompressionMethod
    , entryPath         :: FilePath
    , sourcePath        :: FilePath
    }
  | CopyEntry
    { zipPath     :: FilePath
    , sourcePath  :: FilePath
    , targetPath  :: FilePath
    }
  | PackDirRecur
    { compressionMethod :: CompressionMethod
    , sourcePath        :: FilePath
    , entryPathPrefix   :: Maybe FilePath
    }
  | RenameEntry
    { sourcePath  :: FilePath
    , targetPath  :: FilePath
    }
  | DeleteEntry
    { entryPath :: FilePath
    }
  | Recompress
    { compressionMethod :: CompressionMethod
    , entryPath         :: FilePath
    }
  deriving Show

compressionMethods :: String
compressionMethods = intercalate "|" $ map show ([minBound..maxBound] :: [CompressionMethod])

zipCommand :: Parser ZipCommand
zipCommand = hsubparser $ mconcat
  [ command "CreateArchive"
    (info
      (CreateArchive
        <$> strOption (long "zipPath" <> metavar "FILENAME" <> help "Location of the archive file to create")
      )
      (progDesc "Create a new archive given its location. This will silently overwrite the specified file if it already exists.")
    )
  , command "WithArchive"
    (info
      (WithArchive
        <$> strOption (long "zipPath" <> metavar "FILENAME" <> help "Location of the archive to work with")
      )
      (progDesc "Work with an existing archive.")
    )
  , command "DoesEntryExist"
    (info
      (DoesEntryExist
        <$> strOption (long "entryPath" <> metavar "FILENAME" <> help "Selector that identifies archive entry")
      )
      (progDesc "Check whether the specified entry exists in the archive.")
    )
  , command "GetEntry"
    (info
      (GetEntry
        <$> strOption (long "entryPath" <> metavar "FILENAME" <> help "Selector that identifies archive entry")
      )
      (progDesc "Get contents of a specific archive entry as a strict ByteString. It's not recommended to use this on big entries, because it will suck out a lot of memory.")
    )
  , command "SaveEntry"
    (info
      (SaveEntry
        <$> strOption (long "entryPath" <> metavar "FILENAME" <> help "Selector that identifies archive entry")
        <*> strOption (long "targetPath" <> metavar "FILENAME" <> help "Where to save the file")
      )
      (progDesc "Save a specific archive entry as a file in the file system.")
    )
  , command "CheckEntry"
    (info
      (CheckEntry
        <$> strOption (long "entryPath" <> metavar "FILENAME" <> help "Selector that identifies archive entry")
      )
      (progDesc "Calculate CRC32 check sum and compare it with the value read from the archive. The function returns True when the check sums are the same—that is, the data is not corrupted.")
    )
  , command "UnpackInto"
    (info
      (UnpackInto
        <$> strOption (long "targetPath" <> metavar "DIRNAME" <> help "Where to unpack the archive")
      )
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
    (info
      (AddEntry
        <$> option auto (long "compressionMethod" <> metavar compressionMethods <> help "The compression method to use")
        <*> strOption (long "entryContent" <> metavar "BYTESTRING" <> help "Entry contents")
        <*> strOption (long "entryPath" <> metavar "FILENAME" <> help "Name of the entry to add")
      )
      (progDesc "Add a new entry to the archive given its contents in binary form.")
    )
  , command "LoadEntry"
    (info
      (LoadEntry
        <$> option auto (long "compressionMethod" <> metavar compressionMethods <> help "The compression method to use")
        <*> strOption (long "entryPath" <> metavar "FILENAME" <> help "Name of the entry to add")
        <*> strOption (long "sourcePath" <> metavar "FILENAME" <> help "Path to the file to add")
      )
      (progDesc "Load an entry from a given file.")
    )
  , command "CopyEntry"
    (info
      (CopyEntry
        <$> strOption (long "zipPath" <> metavar "FILENAME" <> help "Path to the archive to copy from")
        <*> strOption (long "sourcePath" <> metavar "FILENAME" <> help "Name of the entry (in the source archive) to copy")
        <*> strOption (long "targetPath" <> metavar "FILENAME" <> help "Name of the entry to insert (in current archive)")
      )
      (progDesc "Copy an entry “as is” from another zip archive.")
    )
  , command "PackDirRecur"
    (info
      (PackDirRecur
        <$> option auto (long "compressionMethod" <> metavar compressionMethods <> help "The compression method to use")
        <*> strOption (long "sourcePath" <> metavar "FILENAME" <> help "Path to the directory to add")
        <*> optional (strOption (long "entryPathPrefix" <> metavar "DIRNAME" <> help "Path prefix of the entries in the archive"))
      )
      (progDesc "Add an directory to the archive. Please note that due to the design of the library, empty sub-directories will not be added.")
    )
  , command "RenameEntry"
    (info
      (RenameEntry
        <$> strOption (long "sourcePath" <> metavar "FILENAME" <> help "The original entry name")
        <*> strOption (long "targetPath" <> metavar "FILENAME" <> help "The new entry name")
      )
      (progDesc "Rename an entry in the archive. If the entry does not exist, nothing will happen.")
    )
  , command "DeleteEntry"
    (info
      (DeleteEntry
        <$> strOption (long "entryPath" <> metavar "FILENAME" <> help "Selector that identifies archive entry")
      )
      (progDesc "Delete an entry from the archive, if it does not exist, nothing will happen.")
    )
  , command "Recompress"
    (info
      (Recompress
        <$> option auto (long "compressionMethod" <> metavar compressionMethods <> help "The new compression method")
        <*> strOption (long "entryPath" <> metavar "FILENAME" <> help "Name of the entry to re-compress")
      )
      (progDesc "Change compression method of an entry, if it does not exist, nothing will happen.")
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
  done - use named options instead of arguments
  done - add field names to command adt constructors
  - add RunZipCommandsFromFile
  - support regex patterns for filtering (i.e. unpack only the selected content)
-}


main :: IO ()
main = do
  let opts = info (many zipCommand <**> helper) fullDesc
  cmds <- execParser opts
  mapM_ print cmds
  pure ()
