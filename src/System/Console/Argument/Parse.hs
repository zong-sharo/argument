module System.Console.Argument.Parse
    ( parseArgs
    , guard'
    , consumeOne
    , satisfy
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
import Control.Monad.Error
import Data.Char (isDigit)


parseArgs :: Argument a -> [String] -> Either ParseError (a, [String])
parseArgs (Argument names parse) xs =
    case break (`elem` names) xs of
         (ys, flag:zs) -> do
             (value, zs') <- parse zs
             return (value, ys ++ zs')
         otherwise     -> throwError $ Recoverable $ show (head names) ++ "is required argument"
parseArgs (Flag names) xs =
    case break (`elem` names) xs of
         (ys, flag:zs) -> return (True, ys ++ zs)
         otherwise     -> return (False, xs)

parseArgs (Map op flag) xs = do
    (value, xs') <- parseArgs flag xs
    return $ (op value, xs')

parseArgs (Ap a b) xs = do
    (op, xs') <- parseArgs a xs
    parseArgs (Map op b) xs'

parseArgs (Pure a) xs = return (a, xs)
parseArgs (Help _ flag) xs = parseArgs flag xs
parseArgs (Metavar _ flag) xs = parseArgs flag xs
parseArgs (Fallback fallback flag) xs =
    catchError (parseArgs flag xs) (\err ->
        case err of
             Recoverable _ -> return (fallback, xs)
             otherwise     -> throwError err)
parseArgs (Optional flag) xs =
    catchError (do
        (value, xs') <- parseArgs flag xs
        return (Just value, xs')) (\err ->
            case err of
                 Recoverable _ -> return (Nothing, xs)
                 otherwise     -> throwError err)

guard' :: Bool -> ParseError -> Either ParseError ()
guard' True err = return ()
guard' False err = throwError err

consumeOne :: [String] -> (String -> Either ParseError a) -> Argument a
consumeOne names parse = Argument names (\xs -> do
    guard' (not (null xs || null (head xs))) (Unrecoverable $ show (head names) ++ "is required argument")
    value <- parse (head xs)
    return (value, tail xs))

satisfy :: [String] -> (String -> Bool) -> Argument String
satisfy names criterion = consumeOne names (\value -> do
    guard' (criterion value) (Unrecoverable $ show value ++ "is not a proper value for the " ++ show (head names))
    return value)

-- | string argument
string :: [String] -> Argument String
string names = consumeOne names return

-- | positive integer argument
natural :: [String] -> Argument Integer
natural names = read `fmap` satisfy names (all isDigit)

-- | integer argument
integer :: [String] -> Argument Integer
integer names = read `fmap` satisfy names (\(x:xs) -> x == '-' || isDigit x && all isDigit xs)

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
(<<) = flip Fallback

infixl 9 =:
-- | add metavar annotation
(=:) :: Argument a -> String -> Argument a
(=:) = flip Metavar

-- | makes argument optional.
option :: Argument a -> Argument (Maybe a)
option = Optional
