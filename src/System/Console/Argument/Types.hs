{-# LANGUAGE GADTs #-}
module System.Console.Argument.Types
    ( Parser
    , Argument(..)
    ) where
import Control.Applicative
import Control.Monad.Error

type Parser a = String -> Either String a

data Argument a where
    Argument :: [String] -> Parser a -> Argument a
    Flag     :: [String] -> Argument Bool
    Optional :: Argument a -> Argument (Maybe a)
    Map      :: (a -> b) -> Argument a -> Argument b
    Ap       :: Argument (a -> b) -> Argument a -> Argument b
    Pure     :: a -> Argument a
    Help     :: String -> Argument a -> Argument a
    Metavar  :: String -> Argument a -> Argument a
    Default  :: Show a => a -> Argument a -> Argument a

instance Functor Argument where
    fmap = Map

instance Applicative Argument where
    pure = Pure
    (<*>) = Ap
