module Luna.Manager.Cmd where

import Prologue
import Options.Applicative as Opts
import Data.Text (Text)

import Control.Monad.State.Layered



------------------------------
-- === Command options  === --
------------------------------

-- === Definition === --

data Options = Options
    { _batchMode :: Bool
    , _command   :: Command
    } deriving (Show)


data Command = Install       InstallOpts
             | MakePackage   MakePackageOpts
             | SwitchVersion SwitchVersionOpts
             | Update
             | Info
             deriving (Show)

data InstallOpts = InstallOpts
    { _selectedComponent        :: Maybe Text
    , _selectedVersion          :: Maybe Text
    , _selectedInstallationPath :: Maybe Text
    } deriving (Show)

data MakePackageOpts = MakePackageOpts
    { _cfgPath :: Text
    } deriving (Show)

data SwitchVersionOpts = SwitchVersionOpts
    { _targetVersion :: Text
    } deriving (Show)

makeLenses ''Options
makeLenses ''InstallOpts
makeLenses ''MakePackageOpts
makeLenses ''SwitchVersionOpts


-- === Instances === --

instance Default InstallOpts where def = InstallOpts def def def



------------------------------
-- === Argument parsing === --
------------------------------

-- === Parsers === --

evalOptionsParserT :: MonadIO m => StateT Options m a -> m a
evalOptionsParserT m = evalStateT m =<< parseOptions

parseOptions :: MonadIO m => m Options
parseOptions = liftIO $ customExecParser (prefs showHelpOnEmpty) optsParser where
    synopsis           = "Luna Manager provides install / uninstall / update and maintain utilities over all Luna related ecosystem components."
    optsParser         = info (helper <*> programOpts) (fullDesc <> header "Luna ecosystem manager" <> progDesc synopsis)
    installCmd         = Opts.command "install"        . info installOpts       $ progDesc "Install components"
    updateCmd          = Opts.command "update"         . info (pure Update)     $ progDesc "Update components"
    switchVersionCmd   = Opts.command "switch-version" . info switchVersionOpts $ progDesc "Switch installed component version"
    mkpkgCmd           = Opts.command "make-package"   . info mkpkgOpts         $ progDesc "Prepare installation package"
    infoCmd            = Opts.command "info"           . info (pure Info)       $ progDesc "Shows environment information"
    commands           = mconcat [installCmd, mkpkgCmd, updateCmd, switchVersionCmd, infoCmd]
    programOpts        = Options           <$> Opts.switch (long "batch" <> help "Do not run interactive mode") <*> hsubparser commands
    mkpkgOpts          = MakePackage       <$> mkpkgOpts'
    mkpkgOpts'         = MakePackageOpts   <$> strArgument (metavar "CONFIG"  <> help "Config file path")
    switchVersionOpts  = SwitchVersion     <$> switchVersionOpts'
    switchVersionOpts' = SwitchVersionOpts <$> strArgument (metavar "VERSION" <> help "Target version")
    installOpts        = Install           <$> installOpts'
    installOpts'       = InstallOpts       <$> (optional . strOption $ long "component" <> short 'c' <> metavar "COMPONENT" <> help "Component to install")
                                           <*> (optional . strOption $ long "version"   <> short 'v' <> metavar "VERSION"   <> help "Version to install"  )
                                           <*> (optional . strOption $ long "path"      <> short 'p' <> metavar "PATH"      <> help "Installation path"   )
