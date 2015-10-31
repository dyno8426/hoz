import Data.Map (Map)
import qualified Data.Map as Map

initializeSAS :: Map Int [String]
initializeSAS = Map.empty

addKeyToSAS :: Int -> Map Int [String] -> Map Int [String]
addKeyToSAS val sas = insert val [""] sas

getValFromSAS :: Int -> Map Int [String] -> String
getValFromSAS key sas = case lookup key sas of
							Just a -> a
							Nothing -> ""

bindValToKeyInSAS :: Int -> String -> Map Int [String] -> Map Int [String]
bindValToKeyInSAS key value sas = insert key value sas

