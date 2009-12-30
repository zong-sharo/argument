module System.Console.Argument.Help
    ( help
    ) where
import System.Console.Argument.Types
import Text.PrettyPrint.HughesPJ
import Data.Maybe (maybe)
import Data.List (isPrefixOf, intercalate)

data Status = IsRequired | IsOptional | WithDefault String
    deriving Show

data ArgumentInfo = ArgumentInfo
    { names    :: [String]
    , status   :: Status
    , metavar  :: Maybe String
    , helpText :: Maybe String
    } deriving Show

argInfo :: Argument a -> [ArgumentInfo]
argInfo (Argument names _)  = [ArgumentInfo names IsRequired Nothing Nothing]
argInfo (Flag names)        = [ArgumentInfo names IsOptional Nothing Nothing]
argInfo (Map op a)          = argInfo a
argInfo (Ap a b)            = argInfo a ++ argInfo b
argInfo (Pure _)            = []
argInfo (Help text a )      = map (\info -> info {helpText = Just text}) (argInfo a)
argInfo (Metavar metavar a) = map (\info -> info {metavar = Just metavar}) (argInfo a)
argInfo (Fallback value a)  = map (\info -> info {status = WithDefault $ show value}) (argInfo a)
argInfo (Optional a)        = map (\info -> info {status = IsOptional}) (argInfo a)

infoPretty :: ArgumentInfo -> Doc
infoPretty info =
    names' $$ nest 25 help
    where
    help = maybe empty text (helpText info) $$ case (status info) of
                                                    IsRequired        -> text "this is required argument"
                                                    IsOptional        -> empty
                                                    WithDefault value -> nest 2 $ text $ "default value is " ++ value
    names' = hsep $ punctuate comma $ short ++ long
    short = map text $ filter isShort $ names info
    long = case (metavar info) of
                Just var -> map (\name -> text (name ++ "=" ++ var)) $ filter (not . isShort) $ names info
                Nothing  -> map text $ filter (not . isShort) $ names info
    isShort = not . isPrefixOf "--"

help :: Argument a -> Doc
help = vcat . map infoPretty . argInfo
