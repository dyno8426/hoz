import SAS
import Environment

data Ident = Ident String deriving (Show, Eq)
data Value = Value String deriving (Show, Eq)
data Statement = Statement [PTree] deriving (Show, Eq)

data PTree = Nop
			| LocalVar Ident Statement
			| BindVarToVar Ident Ident
			| BindVarToVal Ident Value
			| Conditional Ident Statement Statement
			| BindVarToProc Ident Ident Statement
			| Apply Ident Ident String
			deriving (Show, Eq)

main = do
		putStrLn "Hello world!"
