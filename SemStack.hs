module SemStack where
	import qualified Control.Exception as E
	import SAS
	import Environment

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

	evaluateExpression :: Operator -> String -> String -> String
	evaluateExpression Plus x y = show $ (read x :: Int) + (read y :: Int)
	evaluateExpression Minus x y = show $ (read x :: Int) - (read y :: Int)
	evaluateExpression Multiply x y = show $ (read x :: Int) * (read y :: Int)
	evaluateExpression Divide x y = show $ (read x :: Int) `div` (read y :: Int)
	evaluateExpression And "true" "true" = "true"
	evaluateExpression And "false" "true" = "false"
	evaluateExpression And "true" "false" = "false"
	evaluateExpression And "false" "false" = "false"
	evaluateExpression And _ _ = error "Operating 'And' on non-boolean value(s) in SAS."
	evaluateExpression Or "true" "true" = "true"
	evaluateExpression Or "false" "true" = "true"
	evaluateExpression Or "true" "false" = "true"
	evaluateExpression Or "false" "false" = "false"
	evaluateExpression Or _ _ = error "Operating 'Or' on non-boolean value(s) in SAS."
	evaluateExpression EqualEqualTo x y = if x==y then "true" else "false"
	evaluateExpression NotEqualTo x y = if x==y then "false" else "true"

	applyOperation :: Operator -> Store -> Store -> Store
	-- TO DO: INCLUDE ERROR CHECKING - IF THE VALUES x AND y ARE VALID NUMBERS OR NOT
	applyOperation op (Datum x) (Datum y) = Datum $ evaluateExpression op x y
	applyOperation _ _ _ = error "Operating on incompatible values in SAS."

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

	matchAllFeatures :: [(Literal,Int)] -> [(String,Ident)] -> Bool
	matchAllFeatures [] [] = True
	matchAllFeatures [] _ = False
	matchAllFeatures _ [] = False
	matchAllFeatures (((Literal label),key):rest_l_and_k) ((name,value):rest_f_and_v) =	if label==name
																						then matchAllFeatures rest_l_and_k rest_f_and_v
																						else False

	declareRecordValues :: SAS -> EqSets -> Env -> [(String,Ident)] -> (Env,SAS,EqSets)
	declareRecordValues sas eq_sets curr_env [] = (curr_env,sas,eq_sets)
	declareRecordValues sas eq_sets curr_env ((name,(Ident x)):rest_f_and_v) = declareRecordValues new_sas new_eq_sets new_env rest_f_and_v where
																				(new_env,new_sas,new_eq_sets) = Environment.mergeLocalEnv sas eq_sets curr_env x

	bindKeysToValues :: [(Literal,Int)] -> [(String,Ident)] -> SAS -> EqSets -> Env -> (SAS,EqSets)
	bindKeysToValues [] [] sas eq_sets _ = (sas,eq_sets)
	bindKeysToValues (((Literal label),key):rest_l_and_k) ((name,(Ident x)):rest_f_and_v) sas eq_sets curr_env = bindKeysToValues rest_l_and_k rest_f_and_v new_sas new_eq_sets curr_env where
																													(new_sas,new_eq_sets) = SAS.bindKeyToKeyInSAS key key_x sas eq_sets where
																														key_x = Environment.getVarInEnv x curr_env

	doDeclarationAndBinding :: [(Literal,Int)] -> [(String,Ident)] -> Statements -> SemanticStack -> SAS -> EqSets -> Env -> (SemanticStack,SAS,EqSets)
	doDeclarationAndBinding literals_and_keys features_and_values stmts sem_stack sas eq_sets curr_env = (new_sem_stack,new_sas,new_eq_sets) where
																									(new_env,temp_sas,temp_eq_sets) = declareRecordValues sas eq_sets curr_env features_and_values
																									(new_sas,new_eq_sets) = bindKeysToValues literals_and_keys features_and_values temp_sas temp_eq_sets new_env
																									new_sem_stack = pushToSemanticStack sem_stack (reverse stmts) new_env

	patternMatching :: String -> Store -> String -> [(String,Ident)] -> Statements -> Statements -> SemanticStack -> SAS -> EqSets -> Env -> (SemanticStack,SAS,EqSets)
	patternMatching record (Record ((Literal label),literals_and_keys)) name features_and_values stmts_if stmts_else popped_stack sas eq_sets curr_env =	if label==name
																																							then	if (length literals_and_keys)==(length features_and_values)
																																									then	if (matchAllFeatures literals_and_keys features_and_values)
																																											then doDeclarationAndBinding literals_and_keys features_and_values stmts_if popped_stack sas eq_sets curr_env
																																											else ((pushToSemanticStack popped_stack (reverse stmts_else) curr_env),sas,eq_sets)
																																									else ((pushToSemanticStack popped_stack (reverse stmts_else) curr_env),sas,eq_sets)
																																							else ((pushToSemanticStack popped_stack (reverse stmts_else) curr_env),sas,eq_sets)
	patternMatching record _ _ _ _ _ _ _ _ _ = error $ concat ["Incompatible Pattern Matching as '", record, "' is not a Record in SAS."]

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
				val = applyOperation op val_x (Datum v) where
					key_x = Environment.getVarInEnv x curr_env
					val_x = SAS.retrieveFromSAS key_x sas
		OperateWithVar (Ident r) (Ident x) op (Ident y) -> (new_sem_stack,new_sas,eq_sets) where
			new_sem_stack = tail sem_stack
			new_sas = SAS.bindValToKeyInSAS key val sas eq_sets where
				key = Environment.getVarInEnv r curr_env
				val = applyOperation op val_x val_y where
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
		Case (Ident record) name features_and_values stmts_if stmts_else -> patternMatching record val name features_and_values stmts_if stmts_else popped_stack sas eq_sets curr_env where
			key = Environment.getVarInEnv record curr_env
			val = SAS.retrieveFromSAS key sas
			popped_stack = tail sem_stack
		where
			curr_stmt = fst $ popFromSemanticStack sem_stack
			curr_env = snd $ popFromSemanticStack sem_stack

	executeProgram :: SemanticStack -> SAS -> EqSets -> String
	executeProgram sem_stack sas eq_sets = case sem_stack of
		[] -> concat ["SAS: ", show sas, "\n\n", "EqSets: ", show eq_sets]
		top:bottom -> executeProgram new_sem_stack new_sas new_eq_sets where
			(new_sem_stack,new_sas,new_eq_sets) = executeStatement sem_stack sas eq_sets
