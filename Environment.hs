module Environment where
	import qualified Control.Exception as E
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
								Nothing -> error "Variable not declared in the current Environment."

	checkIfVarInEnv :: String -> Env -> Bool
	checkIfVarInEnv key env = Map.member key env

	mergeLocalEnv :: SAS -> EqSets -> Env -> String -> (Env,SAS,EqSets)
	--mergeLocalEnv sas parent_env var = Map.insert var (size new_sas) parent_env where
	--									new_sas = Map.insert (size sas) "NULL" sas
	mergeLocalEnv sas eq_sets parent_env var = (Map.insert var (size sas) parent_env,new_sas,new_eq_sets) where
													(new_sas,new_eq_sets) = SAS.addKeyToSAS (size sas) sas eq_sets

	displayEnv :: Env -> String
	displayEnv = show
