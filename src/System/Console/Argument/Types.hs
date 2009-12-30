{-# LANGUAGE GADTs #-}
module System.Console.Argument.Types
    ( ParseError(..)
    , Argument(..)
    ) where
import Control.Applicative
import Control.Monad.Error

data ParseError
    = Recoverable String
    | Unrecoverable String
    deriving Show

instance Error ParseError where
    noMsg = Recoverable ""
    strMsg = Recoverable

data Argument a where
    Argument :: [String] -> ([String] -> Either ParseError (a, [String])) -> Argument a
    Flag     :: [String] -> Argument Bool
    Map      :: (a -> b) -> Argument a -> Argument b
    Ap       :: Argument (a -> b) -> Argument a -> Argument b
    Pure     :: a -> Argument a
    Help     :: String -> Argument a -> Argument a
    Metavar  :: String -> Argument a -> Argument a
    Fallback :: Show a => a -> Argument a -> Argument a
    Optional :: Argument a -> Argument (Maybe a)

instance Functor Argument where
    fmap = Map

instance Applicative Argument where
    pure = Pure
    (<*>) = Ap
