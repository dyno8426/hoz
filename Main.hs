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
(sas,eq_sets) = SAS.initializeSAS
env = Environment.initializeEnv
sem_stack = pushToSemanticStack [] (reverse program_8) env

main = do
	putStrLn $ executeProgram sem_stack sas eq_sets
	putStrLn "Hello world!"
