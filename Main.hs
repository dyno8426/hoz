import SAS
import Environment

data Ident = Ident String deriving (Show, Eq)
data Value = Value String deriving (Show, Eq)
data Operand = Plus | Minus | Multiply | Divide deriving (Show, Eq)
--data Statements = Statements [PTree] deriving (Show, Eq)

type Statements = [PTree]
type SemanticStack = [(PTree,Env)]

data PTree = Nop
			| LocalVar Ident Statements
			| BindVarToVar Ident Ident
			| BindVarToVal Ident Value
			| Conditional Ident Statements Statements
			| BindVarToProc Ident Ident Statements
			| Apply Ident Ident String
			| OperateWithVar Ident Ident Operand Ident
			| OperateWithVal Ident Ident Operand Value
			deriving (Show, Eq)

pushToSemanticStack :: SemanticStack -> Statements -> Env -> SemanticStack
pushToSemanticStack sem_stack [] _ = sem_stack
pushToSemanticStack sem_stack (stmt:stmts) env = pushToSemanticStack ((stmt,env):sem_stack) stmts env

popFromSemanticStack :: SemanticStack -> (PTree,Env)
popFromSemanticStack = head

executeStatement :: SemanticStack -> SAS -> (SemanticStack,SAS)
executeStatement sem_stack sas = case curr_stmt of
	Nop -> (new_sem_stack,sas) where
		new_sem_stack = tail sem_stack
	LocalVar (Ident x) stmts -> (new_sem_stack,new_sas) where
		new_sas = SAS.addKeyToSAS (SAS.getSizeOfSAS sas) sas
		new_env = Environment.mergeLocalEnv sas curr_env x
		popped_stack = tail sem_stack
		new_sem_stack = pushToSemanticStack popped_stack (reverse stmts) new_env
	where
		curr_stmt = fst $ popFromSemanticStack sem_stack
		curr_env = snd $ popFromSemanticStack sem_stack

executeProgram :: SemanticStack -> SAS -> String
executeProgram sem_stack sas = case sem_stack of
	[] -> show sas
	top:bottom -> executeProgram new_sem_stack new_sas where
		(new_sem_stack,new_sas) = executeStatement sem_stack sas

program_1 = [LocalVar (Ident "x") [BindVarToVal (Ident "x") (Value "1")], LocalVar (Ident "y") [BindVarToVal (Ident "y") (Value "2")]]
program_2 = [Nop, Nop, Nop]
program_3 = [LocalVar (Ident "x") [LocalVar (Ident "y") [LocalVar (Ident "z") [Nop]]]]
sas = SAS.initializeSAS
env = Environment.initializeEnv
sem_stack = pushToSemanticStack [] (reverse program_3) env

main = do
	putStrLn $ executeProgram sem_stack sas
	putStrLn "Hello world!"
