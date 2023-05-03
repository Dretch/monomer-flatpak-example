module Main (main) where

import Control.Monad (void)
import DBus (Variant, toVariant)
import Data.Default.Class (def)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text, intercalate, pack, unpack)
import Data.Word (Word32)
import Desktop.Portal (AddNotificationOptions (..), Client, GetUserInformationOptions (..), GetUserInformationResults (..), NotificationButton (..), NotificationIcon (..), NotificationPriority (..), OpenFileResults (..), Request, addNotificationOptions)
import Desktop.Portal qualified as Portal
import Monomer
import Monomer.Hagrid
import Paths_monomer_flatpak_example (getDataFileName)
import System.Directory (getCurrentDirectory, getHomeDirectory, listDirectory)
import System.Environment (getEnvironment)
import Prelude hiding (unwords)

data AppModel = AppModel
  { portalClient :: Client,
    fileSystem :: Seq EnvironmentInfo,
    environmentVariables :: Seq EnvironmentInfo,
    alertContents :: AlertContents
  }
  deriving (Eq, Show)

data EnvironmentInfo = EnvironmentInfo
  { key :: Text,
    value :: Text
  }
  deriving (Eq, Show)

data AlertContents
  = AlertNotShown
  | AlertRequestingUserInformation (Request GetUserInformationResults)
  | AlertUserInformation GetUserInformationResults
  | AlertRequestingOpenFile (Request OpenFileResults)
  | AlertOpenFile OpenFileResults
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppInitFinish (Seq EnvironmentInfo) (Seq EnvironmentInfo)
  | GetUserInformation
  | GetUserInformationStart (Request GetUserInformationResults)
  | GetUserInformationFinish GetUserInformationResults
  | OpenFile
  | OpenFileStart (Request OpenFileResults)
  | OpenFileFinish OpenFileResults
  | AddNotification
  | CancelRequest
  | CloseAlert

main :: IO ()
main = do
  regularFontPath <- pack <$> getDataFileName "/fonts/Cantarell/Cantarell-Regular.ttf"
  boldFontPath <- pack <$> getDataFileName "/fonts/Cantarell/Cantarell-Bold.ttf"
  iconPath <- pack <$> getDataFileName "/io.github.Dretch.MonomerFlatpakExample.png"
  portalClient <- Portal.connect
  void (Portal.handleNotificationActionInvoked portalClient handleNotificationAction)
  startApp (initialModel portalClient) handleEvent buildUI (config regularFontPath boldFontPath iconPath)
  where
    initialModel portalClient =
      AppModel
        { portalClient,
          fileSystem = mempty,
          environmentVariables = mempty,
          alertContents = AlertNotShown
        }
    config regularFontPath boldFontPath iconPath =
      [ appTheme darkTheme,
        appWindowTitle "Monomer Flatpak Example",
        appWindowIcon iconPath,
        appWindowState (MainWindowNormal (1000, 800)),
        appFontDef "Regular" regularFontPath,
        appFontDef "Bold" boldFontPath,
        appDisableAutoScale True,
        appInitEvent AppInit
      ]

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = tree
  where
    tree =
      zstack
        [ vstack_
            [childSpacing_ 5]
            [ label "Monomer Flatpak Example"
                `styleBasic` [textSize 40, paddingL 5, paddingT 10, paddingR 10],
              label "This is a demo of the monomer framework running inside the Flatpak sandbox."
                `styleBasic` [padding 5],
              hstack_
                [childSpacing]
                [ label "Portals:",
                  button "Get User Information" GetUserInformation,
                  button "Open File" OpenFile,
                  button "Add Notification" AddNotification
                ]
                `styleBasic` [padding 5],
              hagrid
                [ (textColumn "File System Info" (.key)) {initialWidth = 300},
                  (widgetColumn "" valueColumn) {initialWidth = 700}
                ]
                model.fileSystem,
              hagrid
                [ (textColumn "Environment Variable" (.key)) {initialWidth = 300},
                  (widgetColumn "" valueColumn) {initialWidth = 700}
                ]
                model.environmentVariables
            ],
          -- nodeKey to workaround https://github.com/fjvallarino/monomer/issues/265
          maybeAlert `nodeKey` pack (show model.alertContents)
        ]

    maybeAlert = case model.alertContents of
      AlertNotShown ->
        spacer `nodeVisible` False
      AlertRequestingUserInformation _ ->
        alert_
          CancelRequest
          [titleCaption "Getting User Information"]
          (label "Wait or cancel by closing this alert..." `styleBasic` [padding 20])
      AlertRequestingOpenFile _ ->
        alert_
          CancelRequest
          [titleCaption "Opening File..."]
          (label "Wait or cancel by closing this alert..." `styleBasic` [padding 20])
      AlertUserInformation info ->
        alert_
          CloseAlert
          [titleCaption "Get User Information Response"]
          (userInfoAlertContents info `styleBasic` [padding 20])
      AlertOpenFile results ->
        alert_
          CloseAlert
          [titleCaption "Open File Response"]
          (openFileAlertContents results `styleBasic` [padding 20])

    userInfoAlertContents results =
      vstack_
        [childSpacing]
        [ label "Request successful.",
          label ("User Id: " <> results.id),
          label ("User Name: " <> results.name),
          label ("User Image: " <> fromMaybe "[none]" results.image)
        ]

    openFileAlertContents results =
      vstack_
        [childSpacing]
        [ label "Request successful.",
          label ("Selected URIs: " <> intercalate ", " results.uris),
          label ("Selected Choices: " <> pack (show results.choices))
        ]

valueColumn :: Int -> EnvironmentInfo -> WidgetNode s e
valueColumn _ix model =
  label_ model.value [multiline, ellipsis]

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node model = \case
  AppInit ->
    [ Task $ do
        curDir <- pack <$> getCurrentDirectory
        homeDir <- pack <$> getHomeDirectory
        lsRoot <- pack . unlines <$> listDirectory "/"
        lsCurDir <- pack . unlines <$> listDirectory (unpack curDir)
        lsHomeDir <- pack . unlines <$> listDirectory (unpack homeDir)
        envVars <- Seq.fromList . map (\(k, v) -> EnvironmentInfo (pack k) (pack v)) <$> getEnvironment
        pure $
          AppInitFinish
            ( Seq.fromList
                [ EnvironmentInfo "Current Directory" curDir,
                  EnvironmentInfo "Home Directory" homeDir,
                  EnvironmentInfo "ls /" lsRoot,
                  EnvironmentInfo ("ls " <> curDir) lsCurDir,
                  EnvironmentInfo ("ls " <> homeDir) lsHomeDir
                ]
            )
            envVars
    ]
  AppInitFinish fileSystem environmentVariables ->
    [Model model {fileSystem, environmentVariables}]
  GetUserInformation ->
    [ Producer $ \emit -> do
        res <-
          Portal.getUserInformation
            model.portalClient
            def
              { reason = Just "Allows FlatpakMonomerExample to show user information."
              }
        emit (GetUserInformationStart res)
        Portal.await res >>= \case
          Nothing -> emit CloseAlert
          Just result -> emit (GetUserInformationFinish result)
    ]
  GetUserInformationStart res ->
    [Model model {alertContents = AlertRequestingUserInformation res}]
  GetUserInformationFinish info ->
    [Model model {alertContents = AlertUserInformation info}]
  OpenFile ->
    [ Producer $ \emit -> do
        res <- Portal.openFile model.portalClient def
        emit (OpenFileStart res)
        Portal.await res >>= \case
          Nothing -> emit CloseAlert
          Just result -> emit (OpenFileFinish result)
    ]
  OpenFileStart res ->
    [Model model {alertContents = AlertRequestingOpenFile res}]
  OpenFileFinish results ->
    [Model model {alertContents = AlertOpenFile results}]
  AddNotification ->
    [ Producer $ \_emit -> do
        Portal.addNotification model.portalClient $
          (addNotificationOptions "testNotification")
            { title = Just "Test Notification",
              body = Just "Hello!",
              priority = Just NotificationPriorityLow,
              icon = Just (NotificationIconThemed ["weather-snow", "zoom-in"]),
              defaultAction = Just "testDefaultAction",
              defaultActionTarget = Just (toVariant ("wibble" :: Text, 42 :: Word32)),
              buttons = Just [NotificationButton {label_ = "Click Me", action = "testButtonAction", target = Nothing}]
            }
    ]
  CancelRequest ->
    let cancel = case model.alertContents of
          AlertRequestingOpenFile res ->
            [Producer (const (Portal.cancel res))]
          AlertRequestingUserInformation res ->
            [Producer (const (Portal.cancel res))]
          _ -> []
     in cancel <> [Model model {alertContents = AlertNotShown}]
  CloseAlert ->
    [Model model {alertContents = AlertNotShown}]

handleNotificationAction :: Text -> Text -> Maybe Variant -> IO ()
handleNotificationAction notificationId action actionTarget = do
  putStrLn $ "Received notification action:"
  putStrLn $ "  Notification Id: " <> show notificationId
  putStrLn $ "  Action: " <> show action
  putStrLn $ "  Action Target: " <> show actionTarget
