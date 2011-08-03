-- boilerplate {{{
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
import Cmd
import Control.Arrow
import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Data.ConfigFile (CPError(..), ConfigParser(..))
import Data.Function
import Data.List
import Data.List.Split
import Data.Map (Map(..))
import Data.Maybe
import Data.SGF hiding (Point)
import Data.SGF.Parse (Warning(UnknownPropertyPreserved))
import Network.DGS.Types (DGS, LoginResult(..), MoveResult(..), Point)
import Prelude hiding (catch, log)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath ((</>))
import System.IO
import Text.ParserCombinators.Parsec (parse)
import Text.Parsec.Prim
import qualified Data.ByteString  as BS
import qualified Data.ConfigFile  as Config
import qualified Data.Map         as Map
import qualified Network.DGS      as DGS
import qualified Network.DGS.Game as DGS (Game(Game))
import qualified Network.DGS.Game as Game (Game(..))
-- }}}
-- miscellaneous one-liners {{{
transCatch :: MonadIO m => IO a -> (IOError -> m a) -> m a
transCatch io m = join . liftIO $ catch (liftM return io) (return . m)

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile f txt = withBinaryFile f WriteMode (\hdl -> hPutStr hdl txt)

-- DGS doesn't record encoding information in its SGF files, so just guess
-- latin-1 when outputting to the console. When outputting to file, treat SGF
-- files as binary, since they really are.
latin1Output :: MonadIO m => m ()
latin1Output = liftIO (hSetEncoding stdout latin1)
-- }}}
-- Mode {{{
showMode :: Mode -> String
readMode :: String -> Mode

data Mode  = Batch | Interactive deriving (Eq, Ord, Show, Read, Enum, Bounded)
showMode   = map toLower . show
readMode s = case map toLower s of
    ('b':_) -> Batch
    _       -> Interactive
-- }}}
-- Configuration {{{
data Configuration = Configuration {
    username :: Maybe String,
    password :: Maybe String,
    mode     :: Mode,
    editor   :: String,
    server   :: String,
    dataDir  :: String,
    logLevel :: Maybe LogLevel
    } deriving (Eq, Ord, Show, Read)

defaultConfig :: IO Configuration
defaultConfig = do
    dir <- getAppUserDataDirectory "wyvern"
    return Configuration {
        username = Nothing,
        password = Nothing,
        mode     = Interactive,
        editor   = "cgoban3 #f",
        server   = DGS.production,
        dataDir  = dir,
        logLevel = Nothing
        }
-- }}}
-- Wyvern type {{{
newtype Wyvern a = Wyvern { unWyvern :: StateT Configuration DGS a } deriving (
    Functor,
    Monad,
    MonadIO,
    MonadState Configuration
    )

runWyvern :: Wyvern a -> Configuration -> IO a
runWyvern (Wyvern m) = DGS.browseDGS . fmap fst . runStateT m

dgs :: DGS a -> Wyvern a
dgs = Wyvern . lift
-- }}}
-- logging {{{
data LogLevel = Quiet | Noisy | Raucous deriving (Eq, Ord, Show, Read, Enum, Bounded)

incrementLogLevel :: Configuration -> Configuration
incrementLogLevel conf = conf { logLevel = inc (logLevel conf) } where
    levels = Nothing : map Just [Quiet, Noisy, Raucous, Raucous]
    inc l  = dropWhile (/= l) levels !! 1

log :: LogLevel -> String -> Wyvern ()
log l s = do
    level <- gets logLevel
    when (Just l <= level) (liftIO $ putStrLn s)

whisper, say, shout :: String -> Wyvern ()
[whisper, say, shout] = map log [Quiet, Noisy, Raucous]

warn :: MonadIO m => String -> m ()
warn = liftIO . hPutStrLn stderr . ("Warning: " ++)

die :: MonadIO m => Int -> String -> m a
die n extraInfo  = liftIO (speech >> death) where
    speech       = hPutStrLn stderr message
    death        = exitWith (ExitFailure n)
    defaultError = "Unknown error (#" ++ show n ++ "), please report a bug with this number."
    message      = "Error: " ++ fromMaybe defaultError (lookup n problems) ++ separator ++ extraInfo
    separator    = case extraInfo of
        "" -> ""
        _  -> "\n"

problems :: [(Int, String)]
problems = zip [1..] [
    "Bad command-line parameters",
    "Extraneous command-line parameters",
    "The impossible happened during configuration; please\nreport the following information in a bug report.",
    "Parsing configuration file failed",
    "Wrong username",
    "Wrong password",
    "Unknown problem during login attempt",
    "The server issued a totally unexpected error in response to\nan attempt to make a move.  Please report this in a bug,\nalong with the following error code.",
    "The impossible happened; splitOn returned an empty list!\nPlease report this as a bug, along with your editor setting:"
    ]

succeedString :: String -> Wyvern a
succeedString message = liftIO (speech >> death) where
    speech = hPutStrLn stderr message
    death  = exitWith ExitSuccess
-- }}}
-- getters/setters/misc {{{
getLogLevel :: Wyvern (Maybe LogLevel)
getUsername, getPassword, getEditor, getServer, getDataDir :: Wyvern String
setUsername, setPassword, setEditor, setServer, setDataDir :: String -> Wyvern ()
clearUsername, clearPassword, clearLogLevel :: Wyvern ()

getLogLevel = do
    l <- gets logLevel
    shout $ "Retrieved log level <" ++ maybe "Nothing" show l ++ ">"
    return l

getMode = do
    m <- gets mode
    shout $ "Retrieved mode <" ++ showMode m ++ ">"
    return m

[getUsername, getPassword] = map getX [
    ("Username", username, setUsername, True),
    ("Password", password, setPassword, False)
    ] where
    getX (x, selector, setter, echo)
        = gets selector
      >>= maybe (promptX x setter echo) (shoutX x)

    shoutX x val = do
        shout $ "Retrieved " ++ map toLower x ++ " <" ++ val ++ ">"
        return val

    promptX x setter echo = do
        val <- liftIO $ do
            hSetEcho stdin echo
            putStr $ x ++ ": "
            hFlush stdout
            getLine
        liftIO $ hSetEcho stdin True >> when (not echo) (putStrLn "")
        setter val
        return val

[getEditor, getServer, getDataDir] = zipWith getX
    ["SGF editor", "server", "configuration directory"]
    [editor, server, dataDir] where
    getX x selector = do
        v <- gets selector
        shout $ "Retrieved " ++ x ++ " <" ++ v ++ ">"
        return v

[setUsername, setPassword, setEditor, setServer, setDataDir] = zipWith setX
    ["username", "password", "SGF editor", "server", "configuration directory"]
    [\s conf -> conf { username = Just s },
     \s conf -> conf { password = Just s },
     \s conf -> conf { editor   = s },
     \s conf -> conf { server   = s },
     \s conf -> conf { dataDir  = s }
    ] where
    setX x set s = do
        shout $ concat ["Setting ", x, " to <", s, ">"]
        modify (set s)

[clearUsername, clearPassword, clearLogLevel] = zipWith clearX
    ["username", "password", "verbosity"]
    [\conf -> conf { username = Nothing },
     \conf -> conf { password = Nothing },
     \conf -> conf { logLevel = Nothing }
    ] where
    clearX x clear = do
        shout $ "Resetting " ++ x
        modify clear

setMode :: Mode -> Wyvern ()
setMode m = do
    shout $ "Setting mode to <" ++ showMode m ++ ">"
    modify (\conf -> conf { mode = m })

modifyConfig :: (Configuration -> Configuration) -> Wyvern ()
modifyConfig f = do
    new <- gets f
    put new
    shout $ "Setting configuration to " ++ show new

rm :: FilePath -> Wyvern ()
rm file = do
    dataDir <- getDataDir
    whisper $ "Deleting <" ++ file ++ ">"
    liftIO . removeFile $ dataDir </> file
-- }}}
-- DGS interaction {{{
status :: Wyvern [DGS.Game]
status = do
    server   <- getServer
    username <- getUsername
    whisper  $ "Retrieving games list for <" ++ username ++ "> from <" ++ server ++ ">"
    dgs      $ DGS.statusUser server username

login :: Wyvern (String, String)
login = do
    server   <- getServer
    username <- getUsername
    password <- getPassword
    status   <- dgs (DGS.login server username password)
    case status of
        WrongUsername  -> die 5 $ "Server <" ++ server ++ ">, username <" ++ username ++ ">"
        WrongPassword  -> die 6 $ ""
        LoginProblem s -> die 7 $ "The server says <" ++ s ++ ">"
        LoginSuccess   -> shout "Successfully logged in" >> return (server, username)
-- }}}
-- options {{{
options :: String -> String -> String -> String -> [OptDescr (Maybe (Configuration -> Configuration))]
options m e s c = [
    Option "u"  ["user", "username"    ] (arg (\s conf -> conf { username = Just     s }) "username") "Username",
    Option "p"  ["pass", "password"    ] (arg (\s conf -> conf { password = Just     s }) "password") "Password",
    Option "m"  ["mode"                ] (arg (\s conf -> conf { mode     = readMode s }) "mode"    ) (def m "Play mode"),
    Option "e"  ["editor", "launcher"  ] (arg (\s conf -> conf { editor   = s }) "editor") (def e "SGF editor"),
    Option "s"  ["server", "url"       ] (arg (\s conf -> conf { server   = s }) "server") (def s "URL of the go server"),
    Option "c"  ["configuration", "dir"] (arg (\s conf -> conf { dataDir  = s }) "dir"   ) (def c "Configuration and SGF directory"),
    Option "h?" ["help"   ] (NoArg Nothing                 ) "Show usage information",
    Option "v"  ["verbose"] (NoArg (Just incrementLogLevel)) "Increase verbosity"
    ] where
    def v n = concat [n, " (default ", v, ")"]
    arg = ReqArg . (Just .)

usage :: String -> String -> String -> String -> String -> Wyvern String
usage extra m e s c = do
    name <- liftIO getProgName
    return $ usageInfo (extra ++ "Usage: " ++ name ++ " [options]") (options m e s c) ++ unlines [""
        , "Valid modes are \"" ++ intercalate "\" and \"" (map showMode [minBound..maxBound]) ++ "\"."
        , "The first \"#f\" in the editor line will be changed to the name of an SGF"
        , "    file. If there is no \"#f\", one will be appended for you."
        , "Verbosity can be increased up to three times. The third verbosity level"
        , "    may print your password."
        ]

readArgs :: Wyvern ()
readArgs = do
    say "Parsing command-line arguments"
    args <- liftIO getArgs
    shout $ "Got args " ++ show args
    m <- liftM showMode getMode
    e <- getEditor
    s <- getServer
    c <- getDataDir
    case getOpt Permute (options m e s c) args of
        (_, _, errors@(_:_))  -> usage (concat errors) m e s c >>= die 1
        (_, nonopts@(_:_), _) -> usage (unwords nonopts ++ "\n") m e s c >>= die 2
        (modifiers, _, _)     -> case sequence modifiers of
            Nothing -> usage "" m e s c >>= succeedString
            Just ms -> do
                shout "Successfully read arguments"
                clearLogLevel -- we readArgs twice, so gotta reset this each time
                modifyConfig (foldr (.) id ms)
-- }}}
-- config file {{{
rcName :: FilePath -> FilePath
rcName = (</> "config")

yesConfig :: FilePath -> IO (Either CPError ConfigParser)
yesConfig = Config.readfile Config.emptyCP . rcName

createConfigFile :: FilePath -> ConfigParser -> IO ()
createConfigFile dataDir cp = do
    hSetBuffering stdin NoBuffering
    putStr "Create the configuration file now? [Yn] "
    hFlush stdout
    response <- getChar
    when (response /= '\n') (putChar '\n')
    when (response `elem` "Yy\n")
         (createDirectoryIfMissing True dataDir >>
          writeFile (rcName dataDir) (Config.to_string cp))

noConfig :: Wyvern (Either CPError ConfigParser)
noConfig = do
    dataDir <- getDataDir
    warn $ "File <" ++ rcName dataDir ++ "> does not exist."

    -- get the important bits of the configuration
    username <- getUsername
    password <- getPassword
    mode     <- liftM showMode getMode
    editor   <- getEditor
    server   <- getServer

    -- create the config
    whisper "Creating an in-memory copy of this configuration"
    let set' opt val cp = Config.set cp "DEFAULT" opt val
        ecp = return Config.emptyCP
          >>= set' "username" username
          >>= set' "password" password
          >>= set' "mode"     mode
          >>= set' "editor"   editor
          >>= set' "server"   server

    either (die 3 . show) (liftIO . createConfigFile dataDir) ecp
    return ecp

processConfig :: Either CPError ConfigParser -> Wyvern ()
processConfig (Left (err, location)) = die 4 $ "In " ++ location ++ ":\n" ++ show err
processConfig (Right cp) = do
    whisper "Updating configuration"
    mapM_ (ignoreWarn "section") sections
    mapM_ (ignoreWarn "option" ) (options \\ map fst actions)
    mapM_ proc actions
    where
    ignoreWarn s s'     = warn $ "Ignoring unknown " ++ s ++ " <" ++ s' ++ ">"
    actions             = [("username", setUsername), ("password", setPassword), ("mode", setMode . readMode), ("editor", setEditor), ("server", setServer)]
    sections            = Config.sections cp
    Right options       = Config.options  cp "DEFAULT"
    proc (opt, f)       = case Config.get cp "DEFAULT" opt of
        Right val -> (if opt == "password" then shout else say) ("Read " ++ opt ++ " as <" ++ val ++ ">")
                  >> f val
        Left   _  -> return ()

readConfigFile :: Wyvern ()
readConfigFile = do
    dataDir <- getDataDir
    whisper $ "Attempting to read configuration <" ++ rcName dataDir ++ ">"
    ecp     <- transCatch (yesConfig dataDir) (const noConfig)
    processConfig ecp
-- }}}
-- ByteString wrapper for Parsec {{{
newtype Word8String = Word8String { unString :: BS.ByteString }
instance Monad m => Stream Word8String m Word8 where
    uncons = return . fmap (second Word8String) . BS.uncons . unString
-- }}}
-- SGF file management {{{
-- sample file name: dmwit-violist-520005-20091209.sgf
-- (however, note that uids are allowed to have '-' in them, which
-- complicates things somewhat)
gameIDOf :: FilePath -> [String]
gameIDOf = map reverse . take 1 . drop 1 . sepBy "-" . reverse

hasUsername :: String -> FilePath -> Bool
hasUsername username filename = first || second where
    udash       = username ++ "-"
    dashudash   = "-" ++ udash
    first       = udash     `isPrefixOf` filename
    second      = dashudash `isInfixOf`  filename

validGameID :: FilePath -> Bool
validGameID = foldr (/=) False . map (all isDigit) . gameIDOf

isSGF :: FilePath -> Bool
isSGF = (".sgf" `isSuffixOf`)

empty :: [a] -> b -> b -> b
empty [] v _ = v
empty _  _ v = v

emptyM :: Monad m => [a] -> m b -> m [c] -> m [c]
emptyM xs m = empty xs (m >> return [])

lsSGF :: Wyvern [(FilePath, Collection)]
lsSGF = do
    dataDir <- getDataDir
    whisper $ "Scanning <" ++ dataDir ++ "> for SGF files"
    files'  <- transCatch (getDirectoryContents dataDir)
                          (\_ -> return [])
    mapM_ shout $ "Found files:" : files'
    let files = filter (\f -> isSGF f && validGameID f) files'
    mapM_ say $ "Found well-tagged SGFs:" : files
    emptyM files (say "No files found, no need to ask for username") $ do
        username <- getUsername
        let result = filter (hasUsername username) files
        mapM_ whisper $ "Processing these files:" : result
        parseSGFs result

parseResult :: (Stream stream Identity Word8) => String -> (input -> stream) -> input -> Wyvern (Maybe Collection)
parseResult file cast contents = case parse collection file (cast contents) of
    Left err -> do
        warn    $ "Skipping <" ++ file ++ ">: it does not appear to be a valid SGF file"
        whisper $ "The parse error is: " ++ show err
        return Nothing
    Right (collection, warnings_) -> do
        let warnings = filter (UnknownPropertyPreserved "XM" /=) warnings_
        empty warnings
            (say     ("Game <" ++ file ++ "> parsed cleanly"))
            (whisper ("Game <" ++ file ++ "> has correctable errors") >> mapM_ (say . show) warnings)
        return (Just collection)

parseSGF :: FilePath -> Wyvern (Maybe Collection)
parseSGF file | not (validGameID file) = return Nothing
parseSGF file = do
    dataDir  <- getDataDir
    contents <- liftIO . BS.readFile $ dataDir </> file
    parseResult file Word8String contents

parseSGFs :: [FilePath] -> Wyvern [(FilePath, Collection)]
parseSGFs files = liftM (catMaybes . zipWith (fmap . (,)) files) (mapM parseSGF files)

colorFor :: Map Integer Bool -> FilePath -> Collection -> Maybe (FilePath, Bool, Collection)
colorFor m file coll = do
    gid   <- listToMaybe (gameIDOf file)
    color <- Map.lookup (read gid) m
    return (file, color, coll)

-- only keep games that need a move
filterSGFBatch :: [(FilePath, Collection)] -> Wyvern [(FilePath, Bool, Collection)]
filterSGFBatch collections = emptyM collections (shout "No valid SGF files") $ do
    games       <- status
    let gids    = Map.fromList [(gid, color) | DGS.Game { Game.gid = gid, Game.color = color } <- games]
    shout       $ "Games " ++ show (Map.keys gids) ++ " awaiting a move"
    return . catMaybes . map (uncurry (colorFor gids)) $ collections

handleBatch :: FilePath -> Integer -> String -> String -> Bool -> Collection -> Collection -> Wyvern ()
handleBatch file gid server username color disk network = case collectionDiff disk network of
    Wait   s -> say $ "Skipping <" ++ file ++ ">: " ++ s
    Delete s -> rm file >> say s
    Do   p n -> do
        whisper $ "Making move " ++ show n ++ " in game " ++ show gid
        status <- dgs (DGS.move server gid color p n)
        case status of
            NotYourTurn         -> warnSkip "did you play a move when I wasn't looking?"
            MoveAlreadyPlayed   -> warnSkip "did you play a move when I wasn't looking?"
            IllegalPosition     -> warnSkip "the server claims this causes a ko or some other illegal position."
            MoveSuccess         -> shout "...success"
            _                   -> die 8 $ show status
    where warnSkip s = warn $ "Skipping game <" ++ show gid ++ ">; " ++ s

handleAllBatch :: [(FilePath, Bool, Collection)] -> Wyvern ()
handleAllBatch cs = empty cs (whisper "Not logging in") $ do
    (server, username) <- login
    forM_ cs $ \(file, color, disk) ->
        let gid             = head . map read . gameIDOf $ file
            networkFileName = "network (" ++ show gid ++ ")"
        in     dgs (DGS.sgf server gid True)
           >>= parseResult networkFileName (map (toEnum . fromEnum))
           >>= maybe (return ()) (handleBatch file gid server username color disk)

filterSGFInteractive :: [(FilePath, Collection)] -> Wyvern [Integer]
filterSGFInteractive collections = do
    gidsWaitingMove  <- liftM (map Game.gid) status
    let gidsWithMove = [read gid | (file, _) <- collections, gid <- gameIDOf file]
    return (gidsWaitingMove \\ gidsWithMove)

handleInteractive :: FilePath -> String -> String -> String -> Integer -> Wyvern ()
handleInteractive dataDir editor server username gid = do
    shout       $ "Fetching game <" ++ show gid ++ ">"
    sgfContents <- dgs (DGS.sgf server gid True)
    say         $ "Writing <" ++ tempFileName ++ ">"
    shout       $ "File contents:\n" ++ sgfContents ++ "EOF\n"
    liftIO      $ writeBinaryFile fullName sgfContents
    shout       $ "Wrote file, running editor"
    success     <- command
    case success of
        ExitSuccess   -> shout $ "Successfully ran editor on <" ++ tempFileName ++">"
        ExitFailure n -> warn  $ "Editor exited with unsuccessful exit code <" ++ show n ++ "> when run on <" ++ tempFileName ++ ">"
    where
    tempFileName = intercalate "-" [username, "opponent", show gid, "0.sgf"]
    fullName     = dataDir </> tempFileName
    wrappedName  = "\"" ++ fullName ++ "\""
    command      = case splitOn "#f" editor of
        []  -> die 9 editor
        [e] -> liftIO . system $ unwords [e, wrappedName]
        es  -> liftIO . system $ intercalate wrappedName es

handleAllInteractive :: [Integer] -> Wyvern ()
handleAllInteractive gids = empty gids (whisper "No games awaiting moves, not logging in") $ do
    dataDir             <- getDataDir
    editor              <- getEditor
    (server, username)  <- login
    mapM_ (handleInteractive dataDir editor server username) gids

execute :: Mode -> Wyvern ()
execute Batch       = lsSGF >>= filterSGFBatch       >>= handleAllBatch
execute Interactive = lsSGF >>= filterSGFInteractive >>= handleAllInteractive
-- }}}
-- decision {{{
data Decision = Do Point Point | Wait String | Delete String

collectionDiff :: Collection -> Collection -> Decision
collectionDiff [disk] [network] = gameDiff disk network
collectionDiff _ _ = Wait "More than one game in the collection"

gameDiff :: Game -> Game -> Decision
gameDiff disk network
    | on (/=) size disk network = Delete "File board size does not match DGS board size"
    | otherwise                 = on treeDiff tree disk network

treeDiff :: GameTree -> GameTree -> Decision
treeDiff (TreeGo disk) (TreeGo network) = treeGoDiff disk network
treeDiff _ _ = Wait "Game tree for game type other than go"

treeGoDiff :: TreeGo -> TreeGo -> Decision
treeGoDiff disk network = on (forestDiff (rootLabel disk)) subForest disk network

forestDiff :: NodeGo -> [TreeGo] -> [TreeGo] -> Decision
forestDiff prev disk [       ] = decisionForNodes prev disk
forestDiff prev disk [network] =
    case filter (on (==) rootLabel network) disk of
        [disk] -> treeGoDiff disk network
        _      -> Delete "No move in file matches DGS move"
forestDiff _ _ _ = Wait "True game tree has multiple branches"

decisionForNodes :: NodeGo -> [TreeGo] -> Decision
decisionForNodes _ [ ]   = Delete "Reached end of move tree in file"
decisionForNodes
    (                   GameNode { action = Right Move { move = Just (_, Play p) } }  )
    [Node { rootLabel = GameNode { action = Right Move { move = Just (_, Play c) } } }]
                         = Do p c
decisionForNodes _
    [Node { rootLabel = GameNode { action = Right Move { move = Just (_, Play c) } } }]
                         = Wait "True game tree has non-play node"
decisionForNodes _ [_]   = Wait "Predicted game tree has non-play node"
decisionForNodes _ nodes = Wait "Several valid continuations available"
-- }}}
wyvern = do
    readArgs
    readConfigFile
    readArgs
    dgs DGS.silence
    latin1Output
    getMode >>= execute

main = defaultConfig >>= runWyvern wyvern
