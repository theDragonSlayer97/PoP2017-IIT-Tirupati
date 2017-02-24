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
getNumberToken xs = let str = takeWhile (not.isSpace) xs in
                        case (any (\x -> x == '.' ) str) of True  -> (ConstDouble ((read str)::Double), dropWhile (not.isSpace) xs)
                                                            False -> (ConstInt ((read str)::Int)  , dropWhile (not.isSpace) xs)

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
isOperator ch = any (\x -> x == ch) "+-*/%"

--Function which takes a token as an input and return which operator the token is...
whichOperator :: Token -> String
whichOperator (Operator s) = case s of "+" -> "PLUS"
                                       "-" -> "MINUS"
                                       "*" -> "MULT"
                                       "/" -> "DIV"
                                       "%" -> "MOD"
                                       _   -> "None"

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
-- expr -> expr '+' term | expr '-' term | Identifier '=' expr | term
-- term -> term '*' factor | term '/' factor | factor
-- factor -> '(' expr ')' | identifier | number

term :: [Token] ->  (Tree, [Token])
term toks =
    let (termTree, toks') = term toks
    in
        case whichOperator (getToken toks') of "MULT" -> let (factTree, toks'') = factor (eatToken toks')
                                                         in (ProdNode ((whichOperator.getToken) toks) termTree factTree, toks'' )
                                               "DIV"  -> let (factTree, toks'') = factor (eatToken toks')
                                                         in (ProdNode ((whichOperator.getToken) toks) termTree factTree, toks'' )
                                               _      -> factor toks

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
                                                                         _      -> error $ "Parse error on token" ++ (show (getToken toks))
                        _            -> let (expTree, toks') = expr toks
                                        in case (whichOperator.getToken) toks' of "PLUS" -> let (termTree, toks'') = term (eatToken toks')
                                                                                            in (SumNode ((whichOperator.getToken) toks') expTree termTree, toks'')
                                                                                  "MINUS"-> let (termTree, toks'') = term (eatToken toks')
                                                                                            in (SumNode ((whichOperator.getToken) toks') expTree termTree, toks'')
                                                                                  _      -> term toks

parse:: [Token] -> Tree
parse toks = let (tree, toks') = expr toks
             in
                if null toks' then tree else error $ "parsing error: leftover tokens" ++ (show toks')


main = (print . parse . tokenise) "1+2*3"
