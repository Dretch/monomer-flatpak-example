module Main (main) where

import Data.Default.Class (def)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text, intercalate, pack, unpack)
import Desktop.Portal (GetUserInformationResponse (..), Request)
import Desktop.Portal qualified as Portal
import Monomer
import Monomer.Hagrid
import Paths_monomer_flatpak_example (getDataFileName)
import System.Directory (getCurrentDirectory, getHomeDirectory, listDirectory)
import System.Environment (getEnvironment)
import Prelude hiding (unwords)

data AppModel = AppModel
  { fileSystem :: Seq EnvironmentInfo,
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
  | AlertRequestingUserInformation (Request GetUserInformationResponse)
  | AlertUserInformation GetUserInformationResponse
  | AlertRequestingOpenFile (Request [Text])
  | AlertOpenFile [Text]
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppInitFinish (Seq EnvironmentInfo) (Seq EnvironmentInfo)
  | GetUserInformation
  | GetUserInformationStart (Request GetUserInformationResponse)
  | GetUserInformationFinish GetUserInformationResponse
  | OpenFile
  | OpenFileStart (Request [Text])
  | OpenFileFinish [Text]
  | CancelRequest
  | CloseAlert

main :: IO ()
main = do
  regularFontPath <- pack <$> getDataFileName "/fonts/Cantarell/Cantarell-Regular.ttf"
  boldFontPath <- pack <$> getDataFileName "/fonts/Cantarell/Cantarell-Bold.ttf"
  iconPath <- pack <$> getDataFileName "/io.github.Dretch.MonomerFlatpakExample.png"
  startApp initialModel handleEvent buildUI (config regularFontPath boldFontPath iconPath)
  where
    initialModel =
      AppModel
        { fileSystem = mempty,
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
                  button "Open File" OpenFile
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
      AlertOpenFile files ->
        alert_
          CloseAlert
          [titleCaption "Open File Response"]
          (openFileAlertContents files `styleBasic` [padding 20])

    userInfoAlertContents response =
      vstack_
        [childSpacing]
        [ label "Request successful.",
          label ("User Id: " <> response.userId),
          label ("User Name: " <> response.userName),
          label ("User Image: " <> response.userImage)
        ]

    openFileAlertContents files =
      label ("Selected files: " <> intercalate ", " files)

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
        res <- Portal.getUserInformation "Allows FlatpakMonomerExample to show user information."
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
        res <- Portal.openFile def
        emit (OpenFileStart res)
        Portal.await res >>= \case
          Nothing -> emit CloseAlert
          Just result -> emit (OpenFileFinish result)
    ]
  OpenFileStart res ->
    [Model model {alertContents = AlertRequestingOpenFile res}]
  OpenFileFinish files ->
    [Model model {alertContents = AlertOpenFile files}]
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
