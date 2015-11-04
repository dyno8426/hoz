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
pushToSemanticStack semstack [] _ = semstack
pushToSemanticStack semstack (stmt:stmts) env = pushToSemanticStack ((stmt,env):semstack) stmts env

executeProgram :: SemanticStack -> SAS -> String
executeProgram semstack sas = case semstack of
	[] -> show sas
	top:bottom -> show semstack

program = [LocalVar (Ident "x") [BindVarToVal (Ident "x") (Value "1")],LocalVar (Ident "y") [BindVarToVal (Ident "y") (Value "2")]]
sas = SAS.initializeSAS
env = Environment.initializeEnv
semstack = pushToSemanticStack [] (reverse program) env

main = do
	putStrLn $ executeProgram semstack sas
	putStrLn "Hello world!"
