import Data.Map
import qualified Data.Map as Map

initializeSAS :: Map Int String
initializeSAS = Map.empty

addKeyToSAS :: Int -> Map Int String -> Map Int String
addKeyToSAS key sas = Map.insert key "NULL" sas

retrieveFromSAS :: Int -> Map Int String -> String
retrieveFromSAS key sas = case Map.lookup key sas of
							Just a -> a
							Nothing -> "NULL"

bindValToKeyInSAS :: Int -> String -> Map Int String -> Map Int String
bindValToKeyInSAS key value sas = Map.insert key value sas

sas = fromList([(1,"abc"),(2,"def")])
new_sas = addKeyToSAS 3 sas
str = retrieveFromSAS 3 new_sas

displaySAS

main = do
	putStrLn str
	putStrLn "Hello world!"
