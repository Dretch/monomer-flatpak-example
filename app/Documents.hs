module Documents (documents) where

import Control.Exception (SomeException, catch)
import Control.Monad (forM, forM_, void, when)
import Data.Bool (bool)
import Data.Default.Class (def)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text, intercalate, pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Desktop.Portal (Client, directory)
import Desktop.Portal qualified as Portal
import Desktop.Portal.Documents (DocumentId (..), FileIdentifier (..))
import Desktop.Portal.Documents qualified as Documents
import Desktop.Portal.FileChooser (OpenFileResults)
import Monomer
import Monomer.Hagrid
import System.Directory (doesDirectoryExist, getDirectoryContents)

data DocumentsModel = DocumentsModel
  { portalClient :: Client,
    mountPoint :: FilePath,
    documents :: Seq Document
  }
  deriving (Eq, Show)

data Document = Document
  { id :: DocumentId,
    ls :: Text,
    selected :: Bool
  }
  deriving (Eq, Show)

data DocumentsEvent
  = RefreshDocuments
  | RefreshDocumentsFinish FilePath (Seq Document)
  | SetDocumentSelected DocumentId Bool
  | DeleteSelectedDocuments
  | OpenFile
  | OpenDirectory
  | AddFile
  | ShowAlert {title :: Text, body :: Text}

documents :: (CompParentModel s, CompositeEvent e) => Client -> (Text -> Text -> e) -> WidgetNode s e
documents portalClient parentAlert =
  compositeD_ "MonomerFlatpakExample.Documents" (WidgetValue initialModel) buildUI (handleEvent parentAlert) cfg
  where
    cfg = [onInit RefreshDocuments]
    initialModel =
      DocumentsModel
        { portalClient,
          mountPoint = "...",
          documents = mempty
        }

buildUI :: UIBuilder DocumentsModel DocumentsEvent
buildUI _wenv model =
  vstack_
    [childSpacing]
    [ label_ "This shows the documents outside the sandbox that the app has access to. Open files/directories to make them accessible." [multiline]
        `styleBasic` [paddingV 1],
      hstack_
        [childSpacing]
        [ button "Open File" OpenFile,
          button "Open Directory" OpenDirectory,
          button "Add File (~/.var/app/$appId/data/hello.txt)" AddFile,
          button "Delete Selected" DeleteSelectedDocuments
            `nodeEnabled` (Seq.filter (.selected) model.documents /= mempty)
        ],
      hagrid
        [ widgetColumn "" selectedCell,
          textColumn "Id" (\Document {id = DocumentId di} -> di),
          (widgetColumn "Files" filesCell) {initialWidth = 700}
        ]
        model.documents
        `styleBasic` [height 400, width 800]
    ]

selectedCell :: Int -> Document -> WidgetNode s DocumentsEvent
selectedCell _i doc =
  checkboxV doc.selected (SetDocumentSelected doc.id)

filesCell :: Int -> Document -> WidgetNode s DocumentsEvent
filesCell _i doc =
  label_ doc.ls [multiline]

handleEvent :: (Text -> Text -> ep) -> EventHandler DocumentsModel DocumentsEvent sp ep
handleEvent parentAlert _env _node model = \case
  RefreshDocuments ->
    [ Task $ do
        mountPoint <- Documents.getMountPoint model.portalClient
        docs <- getDocuments mountPoint
        pure (RefreshDocumentsFinish mountPoint docs)
    ]
  RefreshDocumentsFinish mountPoint docs ->
    [Model model {mountPoint, documents = docs}]
  SetDocumentSelected docId selected ->
    [Model model {documents = (\doc -> if doc.id == docId then doc {selected} else doc) <$> model.documents}]
  DeleteSelectedDocuments ->
    [ Producer $ \emit -> do
        catchErrors "Delete Failed" emit $ do
          forM_ model.documents $ \doc -> do
            when doc.selected $ do
              Documents.delete model.portalClient doc.id
        emit RefreshDocuments
    ]
  OpenFile ->
    [ Producer $ \emit -> do
        catchErrors "Open Directory Failed" emit $ do
          req <- Portal.openFile model.portalClient def
          Portal.await req >>= \case
            Nothing -> pure () -- user cancelled dialog
            Just result ->
              emit (ShowAlert "Open File Response" (openFileResponseAlert result))
    ]
  OpenDirectory ->
    [ Producer $ \emit -> do
        catchErrors "Open Directory Failed" emit $ do
          req <- Portal.openFile model.portalClient def {directory = Just True}
          Portal.await req >>= \case
            Nothing -> pure () -- user cancelled dialog
            Just result ->
              emit (ShowAlert "Open Directory Response" (openFileResponseAlert result))
    ]
  AddFile ->
    [ Producer $ \emit -> do
        filePath <- (<> "/hello.txt") <$> Portal.getXdgDataHome
        writeFile filePath "Hello!"
        catchErrors "Add File Failed" emit $ do
          void (Documents.add model.portalClient (DocumentFilePath filePath) True True)
        emit RefreshDocuments
    ]
  ShowAlert {title, body} ->
    [Report (parentAlert title body)]

catchErrors :: Text -> (DocumentsEvent -> IO ()) -> IO () -> IO ()
catchErrors title emit cmd = catch cmd handler
  where
    handler (e :: SomeException) =
      emit (ShowAlert title (pack (show e)))

openFileResponseAlert :: OpenFileResults -> Text
openFileResponseAlert results =
  "Request successful.\n\n"
    <> "Selected Files: "
    <> intercalate ", " (pack <$> results.uris)
    <> "\n"
    <> "Selected Choices: "
    <> pack (show results.choices)

getDocuments :: FilePath -> IO (Seq Document)
getDocuments storePath = do
  paths <- getDirectoryContents storePath
  flip foldMap paths $ \path -> do
    let fullPath = storePath <> "/" <> path
    isDir <- doesDirectoryExist fullPath
    if isDir && path /= "by-app" && path /= "." && path /= ".."
      then do
        ls <- lsRecursive fullPath
        pure (Seq.singleton Document {id = DocumentId (pack path), ls, selected = False})
      else pure mempty

lsRecursive :: FilePath -> IO Text
lsRecursive path = do
  displayPath <- withSlashIfDir path path
  childOutputs <- lsRecursive' "" path ""
  pure . toStrict . Builder.toLazyText $
    displayPath <> "\n" <> childOutputs
  where
    lsRecursive' :: Builder -> FilePath -> FilePath -> IO Builder
    lsRecursive' indent dirPath fileName = do
      let fullPath = dirPath <> "/" <> fileName
      childPaths <- doesDirectoryExist fullPath >>= bool (pure []) (getDirectoryContents fullPath)
      childOutputs <- forM childPaths $ \childPath -> do
        if childPath == "." || childPath == ".."
          then pure ""
          else lsRecursive' (indent <> "    ") fullPath childPath
      displayPath <- withSlashIfDir fullPath fileName
      case fileName of
        "" -> pure (mconcat childOutputs)
        _ -> pure (indent <> displayPath <> "\n" <> mconcat childOutputs)

withSlashIfDir :: FilePath -> FilePath -> IO Builder
withSlashIfDir fullPath displayPath = do
  slash <- bool "" "/" <$> doesDirectoryExist fullPath
  pure (Builder.fromString displayPath <> slash)
