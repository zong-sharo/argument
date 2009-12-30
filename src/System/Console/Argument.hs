module System.Console.Argument
    ( Argument
    , getArguments
    , parse
    , help
    , string
    , natural
    , integer
    , flag
    , option
    , (<?>)
    , (<<)
    , (=:)
    ) where
import System.Console.Argument.Types
import System.Console.Argument.Help
import System.Console.Argument.Parse
import System.IO
import System.Exit
import System.Environment
import Control.Applicative hiding (empty)
import Control.Monad
import Data.List (isPrefixOf, find)
import Text.PrettyPrint.HughesPJ hiding (integer)


helpFlag = flag ["-h", "--help"] <?> "display this help and exit"

getArguments :: Argument a -> (Maybe String) -> IO (a, [String])
getArguments arg usageText = do
    r <- parse arg' `fmap` getArgs

    case r of
         Right ((r', wantHelp), xs) -> do
             when wantHelp $
                 usage ExitSuccess arg' usageText
             return (r', xs)
         Left err -> do
             hPutStrLn stderr $ "error: " ++ err
             usage (ExitFailure 1) arg usageText
    where
    arg' = (,) <$> arg <*> helpFlag

usage :: ExitCode -> Argument a -> (Maybe String) -> IO b
usage code arg usageText = do
    name <- getProgName
    hPutStrLn stderr $ render $  maybe empty text usageText $$ text (name ++ " [FLAG]") $$ nest 2 (help arg)
    exitWith code

parse :: Argument a -> [String] -> Either String (a, [String])
parse arg xs =
    case parseArgs arg (concatMap sanitizeArg xs) of
         Right (value, args')     -> case find (isPrefixOf "-") args' of
                                          Nothing -> Right (value, args')
                                          Just a  -> Left $ "unknown argument " ++ show a
         Left (Recoverable err)   -> Left err
         Left (Unrecoverable err) -> Left err


sanitizeArg :: String -> [String]
sanitizeArg x =
    let (name, xs) = break (=='=') x in
        if not (null xs) && not (null $ tail xs)
           then [name, tail xs]
           else [name]
