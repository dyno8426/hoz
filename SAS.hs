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

	insertIntoAllEqSetKeys :: Bool -> [Int] -> String -> SAS -> SAS
	insertIntoAllEqSetKeys False _ _ _ = error "Binding to an already assigned variable in SAS."
	insertIntoAllEqSetKeys True [] _ sas = sas
	insertIntoAllEqSetKeys True (key:keys) value sas = insertIntoAllEqSetKeys True keys value (Map.insert key value sas)

	bindValToKeyInSAS :: Int -> String -> SAS -> EqSets -> SAS
	--bindValToKeyInSAS key value sas = Map.insert key value sas
	bindValToKeyInSAS key value sas eq_sets = insertIntoAllEqSetKeys if_not_already_assigned all_keys value sas where
													if_not_already_assigned = (retrieveFromSAS key sas)=="NULL"
													all_keys = retrieveFromEqSets key eq_sets

	--mergeEqSets :: EqSets -> Int -> Int -> EqSets
	--mergeEqSets eq_sets key_x key_y = new_eq_sets where
	--	temp_eq_sets = Map.insert (key_x) new_x_set eq_sets where
	--		new_x_set = concat [(retrieveFromEqSets key_x eq_sets), (retrieveFromEqSets key_y eq_sets)]
	--	new_eq_sets = Map.insert (key_y) new_y_set temp_eq_sets where
	--		new_y_set = concat [(retrieveFromEqSets key_y eq_sets), (retrieveFromEqSets key_x eq_sets)]
	mergeEqSetsRecursively :: EqSets -> [Int] -> [Int] -> EqSets
	mergeEqSetsRecursively eq_sets _ [] = eq_sets
	mergeEqSetsRecursively eq_sets merged_eq_set (key:keys) = mergeEqSetsRecursively (Map.insert key merged_eq_set eq_sets) merged_eq_set keys

	findMergedEqSet :: EqSets -> Int -> Int -> [Int]
	findMergedEqSet eq_sets key_x key_y = concat [(retrieveFromEqSets key_x eq_sets), (retrieveFromEqSets key_y eq_sets)]

	bindKeyToKeyInSAS :: Int -> Int -> SAS -> EqSets -> (SAS,EqSets)
	bindKeyToKeyInSAS key_x key_y sas eq_sets = if (retrieveFromSAS key_x sas)=="NULL"
												then	if (retrieveFromSAS key_y sas)=="NULL"
														--then (sas,(mergeEqSets eq_sets key_x key_y))
														--else ((bindValToKeyInSAS key_x (retrieveFromSAS key_y sas) sas eq_sets),(mergeEqSets eq_sets key_x key_y))
														then (sas,(mergeEqSetsRecursively eq_sets (findMergedEqSet eq_sets key_x key_y) (findMergedEqSet eq_sets key_x key_y)))
														else ((bindValToKeyInSAS key_x (retrieveFromSAS key_y sas) sas eq_sets),(mergeEqSetsRecursively eq_sets (findMergedEqSet eq_sets key_x key_y) (findMergedEqSet eq_sets key_x key_y)))
												else	if (retrieveFromSAS key_y sas)=="NULL"
														then ((bindValToKeyInSAS key_y (retrieveFromSAS key_x sas) sas eq_sets),(mergeEqSetsRecursively eq_sets (findMergedEqSet eq_sets key_x key_y) (findMergedEqSet eq_sets key_x key_y)))
														else	if (retrieveFromSAS key_x sas)==(retrieveFromSAS key_y sas)
																then (sas,(mergeEqSetsRecursively eq_sets (findMergedEqSet eq_sets key_x key_y) (findMergedEqSet eq_sets key_x key_y)))
																else error "Binding incompatible values in SAS."

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
