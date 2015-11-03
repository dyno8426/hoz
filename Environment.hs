module Environment where
	import Data.Map
	import qualified Data.Map as Map
	import SAS

	type Env = Map String Int

	initializeEnv :: Env
	initializeEnv = Map.empty

	-- returns the SAS location of the variable
	getVarInEnv :: String -> Env -> Int
	getVarInEnv key env = case Map.lookup key env of
								Just a -> a
								Nothing -> -1

	checkIfVarInEnv :: String -> Env -> Bool
	checkIfVarInEnv key env = Map.member key env

	mergeLocalEnv :: SAS -> Env -> String -> Env
	mergeLocalEnv sas parent_env var = Map.insert var (size new_sas) parent_env where
										new_sas = Map.insert (size sas) "NULL" sas

	