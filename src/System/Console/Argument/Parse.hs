module System.Console.Argument.Parse
    ( ArgumentError(..)
    , parseArgs
    , argument
    , option
    , flag
    , (<?>)
    , (<<)
    , (=:)
    , satisfy
    , string
    , natural
    , integer
    ) where
import System.Console.Argument.Types
import Control.Monad.Error
import Data.Char (isDigit)


data ArgumentError
    = RequiredArgumentError [String]
    | ValueParseError [String] String
    | UnknownArgument String
    deriving Show

instance Error ArgumentError where
    noMsg = RequiredArgumentError []
    strMsg = RequiredArgumentError . (:[])

parseArgs :: Argument a -> [String] -> Either ArgumentError (a, [String])
parseArgs (Argument names parse) xs =
    case break (`elem` names) xs of
         (ys, name:z:zs) -> case parse z of
                                 Left msg    -> Left $ ValueParseError names msg
                                 Right value -> Right (value, ys ++ zs)
         otherwise       -> throwError $ RequiredArgumentError names
parseArgs (Flag names) xs =
    case break (`elem` names) xs of
         (ys, name:zs) -> return (True, ys ++ zs)
         otherwise     -> return (False, xs)

parseArgs (Map op arg) xs = do
    (value, xs') <- parseArgs arg xs
    return $ (op value, xs')

parseArgs (Ap a b) xs = do
    (op, xs') <- parseArgs a xs
    parseArgs (Map op b) xs'

parseArgs (Pure a) xs = return (a, xs)
parseArgs (Help _ arg) xs = parseArgs arg xs
parseArgs (Metavar _ arg) xs = parseArgs arg xs
parseArgs (Default value arg) xs =
    catchError (parseArgs arg xs) (\err ->
        case err of
             RequiredArgumentError _ -> return (value, xs)
             otherwise               -> throwError err)
parseArgs (Optional arg) xs =
    catchError (do
        (value, xs') <- parseArgs arg xs
        return (Just value, xs')) (\err ->
            case err of
                 RequiredArgumentError _ -> return (Nothing, xs)
                 otherwise               -> throwError err)

satisfy :: (String -> Bool) -> String -> Either String String
satisfy predicate value | predicate value = Right value
                        | otherwise       = Left value

-- | required argument, fails if argument is not present
argument :: [String] -> Parser a -> Argument a
argument = Argument

-- | optional argument, returns nothing if parser fails
option :: [String] -> Parser a -> Argument (Maybe a)
option names parser = Optional $ Argument names parser

-- | flag, default value is @False@
flag :: [String] -> Argument Bool
flag = Flag

infixl 9 <?>
-- | add help text for the argument
(<?>) :: Argument a -> String -> Argument a
(<?>) = flip Help

infixl 9 <<
-- | add default value for the argument. if argument fails, default value is returned
(<<) :: Show a => Argument a -> a -> Argument a
(<<) = flip Default

infixl 9 =:
-- | add metavar annotation
(=:) :: Argument a -> String -> Argument a
(=:) = flip Metavar

-- | string parser, consumes any string
string :: Parser String
string = return

-- | positive integer parser
natural :: Parser Integer
natural = (fmap . fmap) read (satisfy (all isDigit))

-- | integer parser
integer :: Parser Integer
integer = (fmap . fmap) read (satisfy (\(x:xs) -> x == '-' || isDigit x && all isDigit xs))
