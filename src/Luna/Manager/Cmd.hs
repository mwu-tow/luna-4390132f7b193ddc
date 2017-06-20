module Luna.Manager.Cmd where

import Prologue
import Options.Applicative
import Data.Text (Text)

import Luna.Manager.Installer (InstallOpts, InstallOpts(..))

data Opts = Opts
    { optCommand :: !Command
    }

-- data InstallOptions = InstallOptions { a :: Maybe String
--                                      , b :: Maybe String
--                                      , c :: Maybe String}

data Command = Install InstallOpts
             | MakePackage String
             | Update
             | SwitchVersion String
             | Info



possibleInstallOptions :: Parser InstallOpts
possibleInstallOptions = InstallOpts
           <$> (fmap convert $ optional $ strOption $ long "component" <> short 'c' <> metavar "COMPONENT" <> help "Component to install")
           <*> (fmap convert $ optional $ strOption $ long "version"   <> short 'v' <> metavar "VERSION"   <> help "Version to install"  )
           <*> (fmap convert $ optional $ strOption $ long "path"      <> short 'p' <> metavar "PATH"      <> help "Installation path"   )


managerMenu :: MonadIO m => m ()
managerMenu = do
    opts <- liftIO $ execParser optsParser
    case optCommand opts of
        Install installOpts   -> putStrLn "installation function" --installAppImage
        MakePackage   config  -> putStrLn "make package command" -- mkPackageToRPM config
        Update                -> putStrLn "powinno sciagac nowy config z luna-lang org albo czegos takiego"
        SwitchVersion version -> putStrLn "zmienia versję z aktualnej na inną"
        Info                  -> putStrLn "show actually used version"
    where
        optsParser =
            info
                (helper <*> programOptions)
                (fullDesc <> progDesc "LunaStudio manager" <> header "manager for LunaStudio for making and installing packages")
        programOptions =
            Opts <$> hsubparser (installCommand <> mkpkgCommand <> updateCommand <> switchVersionCommand <> infoCommand)
        installCommand       = command "install"        . info installOptions       $ progDesc "Install Luna component"
        mkpkgCommand         = command "make-package"   . info mkpkgOptions         $ progDesc "Make Luna package"
        updateCommand        = command "update"         . info (pure Update)        $ progDesc "Update Luna components"
        switchVersionCommand = command "switch-version" . info switchVersionOptions $ progDesc "Switch version of Luna components"
        infoCommand          = command "info"           . info (pure Info)          $ progDesc "Shows Info"
        mkpkgOptions         = MakePackage   <$> strArgument (metavar "CONFIG"  <> help "Name of config file")
        switchVersionOptions = SwitchVersion <$> strArgument (metavar "VERSION" <> help "version to switch to")
        installOptions       = Install       <$> possibleInstallOptions
