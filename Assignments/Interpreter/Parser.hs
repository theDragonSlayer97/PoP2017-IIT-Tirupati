import Data.Char
import Data.List
import qualified Data.Map as DM
--Reserved words in julia
listOfReservedWords = ["if",
                       "else",
                       "elseif",
                       "for",
                       "break"
                    ]

--Tokens structure
data Token = Operator String
           | ConstInt Int
           | ConstDouble Double
           | Keyword String
           | LPar
           | RPar
           | Assign
           | Identifier String
           | TokEnd   deriving (Show)

--helper function to tell if any string is contained in another
isSubstring ls hs = isInfixOf ls hs

--Function to return a constant type token from stream and return the rest of stream
getNumberToken :: String -> (Token, String)
getNumberToken xs = let (i, str) = getInteger xs in (ConstInt i, str)

--Function to parse Identifier from input stream
getIdentifier :: String -> (String, String)
getIdentifier xs = (takeWhile isAlphaNum xs, dropWhile isAlphaNum xs)

--Function to parse integer from the input stream
getInteger :: String -> (Int, String)
getInteger xs = ((read (takeWhile isDigit xs))::Int, dropWhile isDigit xs)

--Function to skip spaces while tokenising
skipWhitespace :: String -> String
skipWhitespace xs = dropWhile isSpace xs

--Function to check if character is an Operator
isOperator :: String -> Bool
isOperator oper = any (\x -> x == oper) ["+","-","*","/","%","^","<",">","<=",">=","==","!="]

getOperator :: String -> (Token, String)
getOperator (x:y:ls)
  | isOperator ([x]++[y]) = (Operator ([x]++[y]), ls)
  | isOperator [x] = (Operator [x], [y]++ls)
  | otherwise = error "Some Awkard state happened at getOperator"

--Function which takes a token as an input and return which operator the token is...
whichOperator :: Token -> String
whichOperator (Operator s)
  |s == "+" = "PLUS"
  |s == "-" = "MINUS"
  |s == "*" = "MULT"
  |s == "/" = "DIV"
  |s == "%" = "MOD"
  |otherwise = s
whichOperator _ = "None"

--Checks if the given String matches with a reserved word
isKeyword :: String -> Bool
isKeyword str = any (\x -> x == str) listOfReservedWords


--Tokenise character by character
tokenise :: String -> [Token]
tokenise [] = [TokEnd]
tokenise (x:xs)
  | x =='(' = [LPar] ++ tokenise (xs)
  | x ==')' = [RPar] ++ tokenise (xs)
  | isAlpha x && isKeyword (takeWhile isAlpha (x:xs)) = [Keyword (takeWhile isAlpha (x:xs))] ++ tokenise (dropWhile isAlpha (x:xs))
  | isAlpha x = (\p -> [Identifier (fst p)] ++ tokenise (snd p)) (getIdentifier (x:xs))
  | isDigit x = (\p -> [fst p] ++ tokenise (snd p)) (getNumberToken (x:xs))
  | isSpace x = tokenise (skipWhitespace xs)
  | isOperator [x] || isOperator ([x]++[head xs]) = let (operToken, restOfStream) = getOperator (x:xs) in [operToken] ++ tokenise restOfStream
  | x == '=' = [Assign] ++ tokenise (xs)
  | otherwise = []

--Parse tree Data Structure
data Tree = ANode String Tree Tree
          | RNode String Tree Tree
          | AssignNode String Tree
          | NumNodeDouble Double
          | NumNodeInt Int
          | VarNode String
          deriving Show

--The list of token is basically a stack
--Looking at the current token
getToken :: [Token] -> Token
getToken = head

--Removing the current token from tokenStream
eatToken :: [Token] -> [Token]
eatToken = tail

--hasMoreTokens
hasMoreTokens :: [Token] -> Bool
hasMoreTokens (x : _) = undefined

--Parser - we implement our grammar rules here
-- logicalExpr -> expr [ ('<'|'>'|'<='|'>='|'!='|'==') expr]*
-- expr -> term ['+'|'-' term]* | Identifier '=' expr
-- term -> factor ['*'|'/'|'%' factor]*
-- factor -> '(' expr ')' | identifier | number


helperlogicalExpr :: (Tree, [Token]) -> (Tree, [Token])
helperlogicalExpr (ltree, toks) =
  let (exprTree, toks') = expr (eatToken toks) in
    case whichOperator (getToken toks) of ">"    -> helperlogicalExpr (RNode ">" ltree exprTree, toks')
                                          "<"    -> helperlogicalExpr (RNode "<" ltree exprTree, toks')
                                          ">="   -> helperlogicalExpr (RNode ">=" ltree exprTree, toks')
                                          "<="   -> helperlogicalExpr (RNode "<=" ltree exprTree, toks')
                                          "=="   -> helperlogicalExpr (RNode "==" ltree exprTree, toks')
                                          "!="   -> helperlogicalExpr (RNode "!=" ltree exprTree, toks')
                                          _      -> (ltree, toks)

helperterm :: (Tree, [Token]) -> (Tree, [Token])
helperterm (ltree, toks) =
    let (factTree, toks') = factor (eatToken toks) in
      case whichOperator (getToken toks) of "MULT"-> helperterm (ANode "MULT" ltree factTree, toks')
                                            "DIV" -> helperterm (ANode "DIV" ltree factTree, toks')
                                            "MOD" -> helperterm (ANode "MOD" ltree factTree, toks')
                                            _     -> (ltree, toks)



helperexpr ::(Tree, [Token]) -> (Tree, [Token])
helperexpr (ltree, toks) =
  let (termTree, toks') = term (eatToken toks) in
    case whichOperator (getToken toks) of "PLUS"-> helperexpr (ANode "PLUS" ltree termTree, toks')
                                          "MINUS" -> helperexpr (ANode "MINUS" ltree  termTree, toks')
                                          _     -> (ltree, toks)

term :: [Token] ->  (Tree, [Token])
term toks = helperterm (factor toks)

factor:: [Token] -> (Tree, [Token])
factor toks =
  case getToken toks of  Identifier s     -> (VarNode s, eatToken toks)
                         ConstInt n       -> (NumNodeInt n, eatToken toks)
                         ConstDouble n    -> (NumNodeDouble n, eatToken toks)
                         LPar             -> let (expTree, toks') = logicalExpr (eatToken toks)
                                             in
                                                case (getToken toks') of RPar -> (expTree, eatToken toks')
                                                                         _    -> (error "Missing Right Parenthesis" )
                         _                 -> error $ "Parse error on token" ++ (show (getToken toks))

expr:: [Token] -> (Tree, [Token])
expr toks =
  case getToken toks of Identifier s -> case (getToken.eatToken) toks of  Assign -> let (expTree, toks') = expr ((eatToken.eatToken) toks)
                                                                                    in (AssignNode s expTree, toks')
                                                                          _      -> helperexpr (term toks)
                        _            -> helperexpr (term toks)

logicalExpr :: [Token] -> (Tree, [Token])
logicalExpr toks = helperlogicalExpr (expr toks)

parse:: [Token] -> Tree
parse toks = let (tree, toks') = logicalExpr toks
             in
                if (show (getToken toks')) == "TokEnd" then tree else error $ "parsing error: leftover tokens" ++ (show toks')

type SymTab = DM.Map String Int


evalTree:: Tree -> SymTab -> (Int,SymTab)
--Multiple relational operations can be parsed but can't be executed
evalTree (VarNode x) symtab =
   case DM.lookup x symtab of Just v -> (v,symtab)
                              Nothing -> error $ "undefined variable:" ++ x
evalTree (NumNodeInt x) symtab = (x,symtab)
evalTree (ANode op leftTree rightTree) symtab = case op of "PLUS" -> ((fst(evalTree leftTree symtab)) + (fst(evalTree rightTree symtab)),symtab)
                                                           "MINUS"-> ((fst(evalTree leftTree symtab)) - (fst(evalTree rightTree symtab)),symtab)
                                                           "MULT" -> ((fst(evalTree leftTree symtab)) * (fst(evalTree rightTree symtab)),symtab)
                                                           "DIV"  -> ((fst(evalTree leftTree symtab)) `div` (fst(evalTree rightTree symtab)),symtab)
                                                           "MOD"  -> ((fst(evalTree leftTree symtab)) `rem` (fst(evalTree rightTree symtab)),symtab)
                                                           _      -> error "Unkown Operator at ANode eval"
evalTree (RNode op leftTree rightTree) symtab = case op of "<"    -> if ((fst(evalTree leftTree symtab)) < (fst(evalTree rightTree symtab))) then (1,symtab) else (0,symtab)
                                                           ">"    -> if ((fst(evalTree leftTree symtab)) > (fst(evalTree rightTree symtab))) then (1,symtab) else (0,symtab)
                                                           "<="   -> if ((fst(evalTree leftTree symtab)) <= (fst(evalTree rightTree symtab))) then (1,symtab) else (0,symtab)
                                                           ">="   -> if ((fst(evalTree leftTree symtab)) >= (fst(evalTree rightTree symtab))) then (1,symtab) else (0,symtab)
                                                           "=="   -> if ((fst(evalTree leftTree symtab)) == (fst(evalTree rightTree symtab))) then (1,symtab) else (0,symtab)
                                                           "!="   -> if ((fst(evalTree leftTree symtab)) /= (fst(evalTree rightTree symtab))) then (1,symtab) else (0,symtab)
                                                           _      -> error "Unkown Operator at RNode eval"

evalTree (AssignNode string tree) symtab = let (val,symtab') = (evalTree tree symtab) in (val,(DM.insert string val symtab'))
evalTree _ symtab= error "Awkward evaluation state"

loop :: SymTab -> [String] -> IO(Int)
loop _ [] = return 1
loop symtab program =
  let (val,symTab') = evalTree ((parse.tokenise) (head program)) symtab
    in do
      print val
      loop symTab' (tail program)
main = do
  let program = ["x=1+2",
                  "y=7*x*x+x",
                  "y+6*9"
                  ] -- This holds our program as in...
  loop (DM.fromList []) program
