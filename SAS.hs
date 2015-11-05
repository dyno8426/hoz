module SAS where
	import qualified Control.Exception as E
	import Data.Map
	import qualified Data.Map as Map

	type SAS = Map Int String
	type EqSets = Map Int [Int]

	initializeSAS :: (SAS,EqSets)
	initializeSAS = (Map.empty,Map.empty)

	getSizeOfSAS :: SAS -> Int
	getSizeOfSAS = size

	addKeyToSAS :: Int -> SAS -> EqSets -> (SAS,EqSets)
	addKeyToSAS key sas eq_sets = (new_sas,new_eq_sets) where
										new_sas = Map.insert key "NULL" sas
										new_eq_sets = Map.insert key [key] eq_sets

	retrieveFromSAS :: Int -> SAS -> String
	retrieveFromSAS key sas = case Map.lookup key sas of
								Just a -> a
								Nothing -> error "Segmentation fault: Beyond allocated memory access in SAS."

	retrieveFromEqSets :: Int -> EqSets -> [Int]
	retrieveFromEqSets key eq_sets = case Map.lookup key eq_sets of
										Just b -> b
										Nothing -> error "Segmentation fault: Beyond allocated memory access in EqSets."

	insertIntoAllEqSetKeys :: [Int] -> String -> SAS -> SAS
	insertIntoAllEqSetKeys [] _ sas = sas
	insertIntoAllEqSetKeys (key:keys) value sas = insertIntoAllEqSetKeys keys value (Map.insert key value sas)

	bindValToKeyInSAS :: Int -> String -> SAS -> EqSets -> SAS
	--bindValToKeyInSAS key value sas = Map.insert key value sas
	bindValToKeyInSAS key value sas eq_sets = insertIntoAllEqSetKeys all_keys value sas where
													all_keys = retrieveFromEqSets key eq_sets

	displaySAS :: SAS -> String
	displaySAS = show

	--sas = initializeSAS
	--new_sas = addKeyToSAS (size sas) sas
	--newer_sas = addKeyToSAS (size new_sas) new_sas
	--str = retrieveFromSAS 2 new_sas
	--sas_str = displaySAS newer_sas

	--main = do
	--	putStrLn str
	--	putStrLn $ show new_sas
	--	putStrLn $ show newer_sas
	--	putStrLn "Hello world!"
