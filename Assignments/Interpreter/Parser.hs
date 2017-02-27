--Imports
import Data.Char

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

--Function to return a constant type token from stream and return the rest of stream
getNumberToken :: String -> (Token, String)
getNumberToken xs = let (i, str) = getInteger xs
                    in case (head str == '.') of
                      True -> (ConstDouble ((read ((show i) ++ "." ++ (takeWhile isDigit (tail str))))::Double), (dropWhile isDigit (tail str)))
                      False -> (ConstInt i, str)


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
isOperator :: Char -> Bool
isOperator ch = any (\x -> x == ch) "+-*/"

--Function which takes a token as an input and return which operator the token is...
whichOperator :: Token -> String
whichOperator (Operator s)
  |s == "+" = "PLUS"
  |s == "-" = "MINUS"
  |s == "*" = "MULT"
  |s == "/" = "DIV"
  |otherwise = "None"
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
  | x == '=' = [Assign] ++ tokenise (xs)
  | isAlpha x = (\p -> [Identifier (fst p)] ++ tokenise (snd p)) (getIdentifier (x:xs))
  | isDigit x = (\p -> [fst p] ++ tokenise (snd p)) (getNumberToken (x:xs))
  | isSpace x = tokenise (skipWhitespace xs)
  | isOperator x = [Operator [x]] ++ tokenise (xs)
  | otherwise = []

--Parse tree Data Structure
data Tree = SumNode String Tree Tree
          | ProdNode String Tree Tree
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
-- expr -> term ['+'|'-' expr]* | Identifier '=' expr
-- term -> factor ['*'|'/' term]*
-- factor -> '(' expr ')' | identifier | number

term :: [Token] ->  (Tree, [Token])
term toks =
    let (factTree, toks') = factor toks
    in
        case whichOperator (getToken toks') of "MULT" -> let (termTree', toks'') = term (eatToken toks')
                                                         in (ProdNode "MULT" factTree termTree', toks'' )
                                               "DIV"  -> let (termTree', toks'') = term (eatToken toks')
                                                         in (ProdNode "DIV" factTree termTree', toks'' )
                                               _      -> (factTree, toks')

factor:: [Token] -> (Tree, [Token])
factor toks =
  case getToken toks of  Identifier s     -> (VarNode s, eatToken toks)
                         ConstInt n       -> (NumNodeInt n, eatToken toks)
                         ConstDouble n    -> (NumNodeDouble n, eatToken toks)
                         LPar             -> let (expTree, toks') = expr (eatToken toks)
                                             in
                                                case (getToken toks') of RPar -> (expTree, eatToken toks')
                                                                         _    -> (error "Missing Right Parenthesis" )
                         _                 -> error $ "Parse error on token" ++ (show (getToken toks))

expr:: [Token] -> (Tree, [Token])
expr toks =
  case getToken toks of Identifier s -> case (getToken.eatToken) toks of Assign -> let (expTree, toks') = expr ((eatToken.eatToken) toks)
                                                                                   in (AssignNode s expTree, toks')
                                                                         _      -> let (termTree, toks') = term toks
                                                                                         in case (whichOperator.getToken) toks' of "PLUS" -> let (exprTree', toks'') = expr (eatToken toks')
                                                                                                                                             in (SumNode ((whichOperator.getToken) toks') termTree exprTree', toks'')
                                                                                                                                   "MINUS"-> let (exprTree', toks'') = expr (eatToken toks')
                                                                                                                                             in (SumNode ((whichOperator.getToken) toks') termTree exprTree', toks'')
                                                                                                                                   _      -> (termTree, toks')
                        _            -> let (termTree, toks') = term toks
                                        in case (whichOperator.getToken) toks' of "PLUS" -> let (exprTree', toks'') = expr (eatToken toks')
                                                                                            in (SumNode ((whichOperator.getToken) toks') termTree exprTree', toks'')
                                                                                  "MINUS"-> let (exprTree', toks'') = expr (eatToken toks')
                                                                                            in (SumNode ((whichOperator.getToken) toks') termTree exprTree', toks'')
                                                                                  _      -> (termTree, toks')

parse:: [Token] -> Tree
parse toks = let (tree, toks') = expr toks
             in
                if (show (getToken toks')) == "TokEnd" then tree else error $ "parsing error: leftover tokens" ++ (show toks')

evalTree:: Tree -> Int
evalTree (NumNodeInt x) = x
evalTree (SumNode op leftTree rightTree) = case op of "PLUS" -> (evalTree leftTree + evalTree rightTree)
                                                      "MINUS"-> (evalTree leftTree - evalTree rightTree)
                                                      _      -> error "Unknown Operator at SumNode"
evalTree (ProdNode op leftTree rightTree) = case op of "MULT" -> (evalTree leftTree * evalTree rightTree)
                                                       "DIV"  -> (evalTree leftTree `div` evalTree rightTree)
                                                       _      -> error "Unkown Operator at ProdNode"
evalTree _ = error "Awkward evaluation state"


main = do
  print (evalTree (parse (tokenise " 1 / 2 ")))
