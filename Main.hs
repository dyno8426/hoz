import qualified Control.Exception as E
import SAS
import Environment

--data Ident = Ident String deriving (Show, Eq)
--data Value = Value String deriving (Show, Eq)
--data Operator = Plus | Minus | Multiply | Divide deriving (Show, Eq)
----data Statements = Statements [PTree] deriving (Show, Eq)

--type Statements = [PTree]
--type SemanticStack = [(PTree,Env)]

--data PTree = Nop
--			| LocalVar Ident Statements
--			| BindVarToVar Ident Ident
--			| BindVarToVal Ident Value
--			| Conditional Ident Statements Statements
--			| BindVarToProc Ident [Ident] Statements
--			| Apply Ident [Ident]
--			| OperateWithVar Ident Ident Operator Ident
--			| OperateWithVal Ident Ident Operator Value
--			deriving (Show, Eq)

pushToSemanticStack :: SemanticStack -> Statements -> Env -> SemanticStack
pushToSemanticStack sem_stack [] _ = sem_stack
pushToSemanticStack sem_stack (stmt:stmts) env = pushToSemanticStack ((stmt,env):sem_stack) stmts env

popFromSemanticStack :: SemanticStack -> (PTree,Env)
popFromSemanticStack = head

selectStatements :: Store -> Statements -> Statements -> SemanticStack -> SAS -> EqSets -> Env -> (SemanticStack,SAS,EqSets)
selectStatements (Datum "true") stmts_if stmts_else sem_stack sas eq_sets curr_env = (new_sem_stack,sas,eq_sets) where
																				popped_stack = tail sem_stack
																				new_sem_stack = pushToSemanticStack popped_stack (reverse stmts_if) curr_env
selectStatements (Datum "false") stmts_if stmts_else sem_stack sas eq_sets curr_env = (new_sem_stack,sas,eq_sets) where
																				popped_stack = tail sem_stack
																				new_sem_stack = pushToSemanticStack popped_stack (reverse stmts_else) curr_env
selectStatements _ _ _ _ _ _ _ = error "Boolean value required for a Conditional clause."

evaluateExpression :: Operator -> Int -> Int -> Int
evaluateExpression Plus x y = x + y
evaluateExpression Minus x y = x - y
evaluateExpression Multiply x y = x * y
evaluateExpression Divide x y = x `div` y

applyArithmeticOperation :: Operator -> Store -> Store -> Store
-- TO DO: INCLUDE ERROR CHECKING - IF THE VALUES x AND y ARE VALID NUMBERS OR NOT
applyArithmeticOperation op (Datum x) (Datum y) = Datum $ show (evaluateExpression op (read x :: Int) (read y :: Int))
applyArithmeticOperation _ _ _ = error "Operating on incompatible values in SAS."

findFromEnvironment :: Env -> [Ident] -> [Int] -> [Int]
findFromEnvironment _ [] keys = keys
findFromEnvironment curr_env ((Ident x):idents) keys = findFromEnvironment curr_env idents (concat [keys,[Environment.getVarInEnv x curr_env]])

declareProcedureArguments :: SAS -> EqSets -> Env -> [Ident] -> (Env,SAS,EqSets)
declareProcedureArguments sas eq_sets proc_env [] = (proc_env,sas,eq_sets)
declareProcedureArguments sas eq_sets proc_env ((Ident x):idents) = declareProcedureArguments new_sas new_eq_sets new_proc_env idents where
																	(new_proc_env,new_sas,new_eq_sets) = Environment.mergeLocalEnv sas eq_sets proc_env x

bindCurrAndProcArguments :: String -> [Int] -> [Int] -> SAS -> EqSets -> (SAS,EqSets)
bindCurrAndProcArguments _ [] [] sas eq_sets = (sas,eq_sets)
bindCurrAndProcArguments proc _ [] _ _ = error $ concat ["More than required arguments supplied for Procedure '", proc, "'."]
bindCurrAndProcArguments proc [] _ _ _ = error $ concat ["Less than required arguments supplied for Procedure '", proc, "'."]
bindCurrAndProcArguments proc (c_key:c_keys) (p_key:p_keys) sas eq_sets = bindCurrAndProcArguments proc c_keys p_keys new_sas new_eq_sets where
																			(new_sas,new_eq_sets) = SAS.bindKeyToKeyInSAS c_key p_key sas eq_sets

checkAndExtractComponents :: String -> Store -> ([Ident],Statements,Env)
checkAndExtractComponents _ (Proc (a,b,c)) = (a,b,c)
checkAndExtractComponents proc _ = error $ concat ["'", proc, "' is not a defined Procedure in SAS."]

getKeysForFeatures :: [(String,Ident)] -> Env -> [(Literal,Int)] -> [(Literal,Int)]
getKeysForFeatures [] _ l_and_k = l_and_k
getKeysForFeatures ((feature,(Ident x)):tuples) curr_env l_and_k = getKeysForFeatures tuples curr_env (concat [l_and_k,[((Literal feature),key)]]) where
																key = Environment.getVarInEnv x curr_env

executeStatement :: SemanticStack -> SAS -> EqSets -> (SemanticStack,SAS,EqSets)
executeStatement sem_stack sas eq_sets = case curr_stmt of
	Nop -> (new_sem_stack,sas,eq_sets) where
		new_sem_stack = tail sem_stack
	LocalVar (Ident x) stmts -> (new_sem_stack,new_sas,new_eq_sets) where
		--new_sas = SAS.addKeyToSAS (SAS.getSizeOfSAS sas) sas
		(new_env,new_sas,new_eq_sets) = Environment.mergeLocalEnv sas eq_sets curr_env x
		popped_stack = tail sem_stack
		new_sem_stack = pushToSemanticStack popped_stack (reverse stmts) new_env
	BindVarToVal (Ident x) (Value v) -> (new_sem_stack,new_sas,eq_sets) where
		-- TO DO: INCLUDE ERROR CHECKING - IF THE IDENTIFER x HAS ALREADY BEEN ASSIGNED A VALUE
		new_sem_stack = tail sem_stack
		new_sas = SAS.bindValToKeyInSAS key (Datum v) sas eq_sets where
			key = Environment.getVarInEnv x curr_env
			--if Environment.checkIfVarInEnv x curr_env
			--	then key = Environment.getVarInEnv x curr_env
			--	else error x ++ " not declared"
	BindVarToVar (Ident x) (Ident y) -> (new_sem_stack,new_sas,new_eq_sets) where
		new_sem_stack = tail sem_stack
		(new_sas,new_eq_sets) = SAS.bindKeyToKeyInSAS key_x key_y sas eq_sets where
			key_x = Environment.getVarInEnv x curr_env
			key_y = Environment.getVarInEnv y curr_env
	Conditional (Ident x) stmts_if stmts_else -> selectStatements val stmts_if stmts_else sem_stack sas eq_sets curr_env where
		key = Environment.getVarInEnv x curr_env
		val = SAS.retrieveFromSAS key sas
	BindVarToProc (Ident x) args stmts -> (new_sem_stack,new_sas,eq_sets) where
		new_sem_stack = tail sem_stack
		new_sas = SAS.bindValToKeyInSAS key val sas eq_sets where
			key = Environment.getVarInEnv x curr_env
			val = Proc (args,stmts,curr_env)
	Apply (Ident proc) args -> (new_sem_stack,new_sas,new_eq_sets) where
		curr_args_keys = findFromEnvironment curr_env args []
		(proc_args,proc_stmts,proc_env) = checkAndExtractComponents proc proc_def where
			key = Environment.getVarInEnv proc curr_env
			proc_def = SAS.retrieveFromSAS key sas
		(temp_env,temp_sas,temp_eq_sets) = declareProcedureArguments sas eq_sets proc_env proc_args
		proc_args_keys = findFromEnvironment temp_env proc_args []
		(new_sas,new_eq_sets) = bindCurrAndProcArguments proc curr_args_keys proc_args_keys temp_sas temp_eq_sets
		popped_stack = tail sem_stack
		new_sem_stack = pushToSemanticStack popped_stack (reverse proc_stmts) temp_env
	OperateWithVal (Ident r) (Ident x) op (Value v) -> (new_sem_stack,new_sas,eq_sets) where
		new_sem_stack = tail sem_stack
		new_sas = SAS.bindValToKeyInSAS key val sas eq_sets where
			key = Environment.getVarInEnv r curr_env
			val = applyArithmeticOperation op val_x (Datum v) where
				key_x = Environment.getVarInEnv x curr_env
				val_x = SAS.retrieveFromSAS key_x sas
	OperateWithVar (Ident r) (Ident x) op (Ident y) -> (new_sem_stack,new_sas,eq_sets) where
		new_sem_stack = tail sem_stack
		new_sas = SAS.bindValToKeyInSAS key val sas eq_sets where
			key = Environment.getVarInEnv r curr_env
			val = applyArithmeticOperation op val_x val_y where
				key_x = Environment.getVarInEnv x curr_env
				val_x = SAS.retrieveFromSAS key_x sas
				key_y = Environment.getVarInEnv y curr_env
				val_y = SAS.retrieveFromSAS key_y sas
	BindVarToRec (Ident x) name features_and_values -> (new_sem_stack,new_sas,eq_sets) where
		new_sem_stack = tail sem_stack
		new_sas = SAS.bindValToKeyInSAS key val sas eq_sets where
			key = Environment.getVarInEnv x curr_env
			val = Record ((Literal name),literals_and_keys) where
				literals_and_keys = getKeysForFeatures features_and_values curr_env []
	where
		curr_stmt = fst $ popFromSemanticStack sem_stack
		curr_env = snd $ popFromSemanticStack sem_stack

executeProgram :: SemanticStack -> SAS -> EqSets -> String
executeProgram sem_stack sas eq_sets = case sem_stack of
	[] -> concat ["SAS: ", show sas, "\n", "EqSets: ", show eq_sets]
	top:bottom -> executeProgram new_sem_stack new_sas new_eq_sets where
		(new_sem_stack,new_sas,new_eq_sets) = executeStatement sem_stack sas eq_sets

program_1 = [LocalVar (Ident "x") [BindVarToVal (Ident "x") (Value "1")], LocalVar (Ident "y") [BindVarToVal (Ident "y") (Value "2")]]
program_2 = [Nop, Nop, Nop]
program_3 = [LocalVar (Ident "x") [LocalVar (Ident "y") [LocalVar (Ident "z") [Nop]]]]
program_4 = [LocalVar (Ident "x") [LocalVar (Ident "y") [LocalVar (Ident "x") [LocalVar (Ident "y") [Nop]]]]]
program_5 = [LocalVar (Ident "a") [LocalVar (Ident "b") [BindVarToVal (Ident "a") (Value "3"), BindVarToVal (Ident "b") (Value "4")], Nop], Nop]
program_6 = [LocalVar (Ident "a") [LocalVar (Ident "a") [BindVarToVal (Ident "a") (Value "5"), BindVarToVal (Ident "a") (Value "6")], BindVarToVal (Ident "a") (Value "7")], Nop]
program_7 = [LocalVar (Ident "clause") [LocalVar (Ident "p") [BindVarToVal (Ident "clause") (Value "true"), Conditional (Ident "clause") [BindVarToVal (Ident "p") (Value "clause was true")] [BindVarToVal (Ident "p") (Value "clause was false")]]]]
program_8 = [Nop, LocalVar (Ident "a") [BindVarToVal (Ident "a") (Value "1")], LocalVar (Ident "b") [BindVarToVal (Ident "b") (Value "2"), BindVarToVal (Ident "a") (Value "3")], Nop]
program_9 = [LocalVar (Ident "result") [LocalVar (Ident "variable") [BindVarToVal (Ident "variable") (Value "40"), OperateWithVal (Ident "result") (Ident "variable") Plus (Value "2")]]]
program_10 = [LocalVar (Ident "d") [LocalVar (Ident "e") [LocalVar (Ident "f") [BindVarToVal (Ident "e") (Value "5"), BindVarToVal (Ident "f") (Value "6"), OperateWithVar (Ident "d") (Ident "e") Plus (Ident "f")]]]]
program_11 = [LocalVar (Ident "p") [LocalVar (Ident "q") [BindVarToVar (Ident "p") (Ident "q"), LocalVar (Ident "r") [BindVarToVar (Ident "r") (Ident "p"), BindVarToVal (Ident "q") (Value "42")]]]]
program_12 = [LocalVar (Ident "m") [BindVarToVal (Ident "m") (Value "true"), LocalVar (Ident "n") [BindVarToVal (Ident "n") (Value "false"), LocalVar (Ident "o") [BindVarToVar (Ident "m") (Ident "n")]]]]
program_13 = [LocalVar (Ident "x") [LocalVar (Ident "y") [BindVarToVar (Ident "x") (Ident "y"), BindVarToVal (Ident "x") (Value "true")]], LocalVar (Ident "p") [LocalVar (Ident "q") [BindVarToVal (Ident "q") (Value "false"), BindVarToVar (Ident "p") (Ident "q")]]]
program_14 = [LocalVar (Ident "x") [BindVarToVal (Ident "x") (Value "alice"), LocalVar (Ident "x") [BindVarToVal (Ident "x") (Value "bob"), LocalVar (Ident "x") [BindVarToVal (Ident "x") (Value "alice and bob")]]]]
program_15 = [LocalVar (Ident "proc_1") [LocalVar (Ident "x") [BindVarToVal (Ident "x") (Value "42"), BindVarToProc (Ident "proc_1") [(Ident "x"), (Ident "y")] [OperateWithVal (Ident "y") (Ident "x") Plus (Value "1")]]]]
program_16 = [LocalVar (Ident "add_one") [LocalVar (Ident "x") [BindVarToVal (Ident "x") (Value "41"), BindVarToProc (Ident "add_one") [(Ident "p_y")] [OperateWithVal (Ident "p_y") (Ident "x") Plus (Value "1")], LocalVar (Ident "x") [LocalVar (Ident "y") [BindVarToVal (Ident "x") (Value "99"), Apply (Ident "add_one") [(Ident "y")]]]]]]
program_17 = [LocalVar (Ident "add_x_y") [LocalVar (Ident "x") [BindVarToProc (Ident "add_x_y") [(Ident "y"), (Ident "z")] [OperateWithVar (Ident "z") (Ident "x") Plus (Ident "y")], BindVarToVal (Ident "x") (Value "2")], LocalVar (Ident "x") [LocalVar (Ident "y") [LocalVar (Ident "z") [BindVarToVal (Ident "x") (Value "10"), BindVarToVal (Ident "y") (Value "40"), Apply (Ident "add_x_y") [(Ident "y"), (Ident "z")]]]]]]
program_18 = [LocalVar (Ident "add_x_y") [LocalVar (Ident "x") [BindVarToProc (Ident "add_x_y") [(Ident "y"), (Ident "z")] [OperateWithVar (Ident "z") (Ident "x") Plus (Ident "y")], BindVarToVal (Ident "x") (Value "2")], LocalVar (Ident "x") [LocalVar (Ident "y") [LocalVar (Ident "z") [BindVarToVal (Ident "x") (Value "10"), BindVarToVal (Ident "y") (Value "40"), Apply (Ident "x") [(Ident "y"), (Ident "z")]]]]]]
program_19 = [LocalVar (Ident "someone") [LocalVar (Ident "name") [LocalVar (Ident "id") [BindVarToVal (Ident "id") (Value "786"), BindVarToRec (Ident "someone") "employee" [("name",(Ident "name")), ("id",(Ident "id"))]], BindVarToVal (Ident "name") (Value "ramu")]]]
(sas,eq_sets) = SAS.initializeSAS
env = Environment.initializeEnv
sem_stack = pushToSemanticStack [] (reverse program_19) env

main = do
	putStrLn $ executeProgram sem_stack sas eq_sets
	putStrLn "Hello world!"
