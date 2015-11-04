module SAS where
	import Data.Map
	import qualified Data.Map as Map

	type SAS = Map Int String

	initializeSAS :: SAS
	initializeSAS = Map.empty

	addKeyToSAS :: Int -> SAS -> SAS
	addKeyToSAS key sas = Map.insert key "NULL" sas

	retrieveFromSAS :: Int -> SAS -> String
	retrieveFromSAS key sas = case Map.lookup key sas of
								Just a -> a
								Nothing -> "NULL"

	bindValToKeyInSAS :: Int -> String -> SAS -> SAS
	bindValToKeyInSAS key value sas = Map.insert key value sas

	displaySAS :: SAS -> Int -> Int -> String -> String
	displaySAS sas val n temp = if val==n
								then temp
								else displaySAS sas (val+1) n (temp ++ "(" ++ (show val) ++ "," ++ (retrieveFromSAS val sas) ++ ")")

	--sas = initializeSAS
	--sas = fromList([(0,"abc"),(1,"def")])
	--new_sas = addKeyToSAS (size sas) sas
	--newer_sas = addKeyToSAS (size new_sas) new_sas
	--str = retrieveFromSAS 2 new_sas
	--sas_str = displaySAS new_sas 0 (size new_sas) ""

	--main = do
	--	putStrLn str
	--	putStrLn $ show new_sas
	--	putStrLn $ show newer_sas
	--	putStrLn "Hello world!"
	