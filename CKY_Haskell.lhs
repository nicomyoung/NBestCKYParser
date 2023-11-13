This script implements the CKY (Cocke–Kasami–Younger) 
parsing algorithm in Haskell. 
The CKY algorithm is a type of chart parsing algorithm 
for context-free grammars.

Enable the language extension for Overloaded strings:

> {-# LANGUAGE OverloadedStrings #-}
> 
> module CKY where
> 
> import Control.Monad (forM_)
> import qualified Data.Map as M
> import Data.Map (Map)
> import qualified Data.Text as T
> import Data.Text (Text)
> import Data.List
> import Data.Maybe
> import Text.Parsec (parse)
  
> type Word = Text
> type NonTerminal = Text
> type Terminal = Text
> 
> data Rule = Unary NonTerminal Terminal
>           | Binary NonTerminal NonTerminal NonTerminal
>           | UnaryRule NonTerminal NonTerminal
>   deriving (Show)


> 
> type Grammar = [Rule]
> type Sentence = [Terminal]
> 

 define the ParseTable type, which is a Map from pairs of Int 
 (representing start and end positions in the sentence) 
 to lists of NonTerminal 
 (the possible grammatical categories for the corresponding substring).


> type ParseTable = Map (Int, Int) [NonTerminal]


The cky function implements the CKY algorithm. 
It first initializes the table with the terminal symbols, 
then applies the binary rules, 
and finally the unary rules that produce non-terminal symbols.


> cky :: Grammar -> Sentence -> ParseTable
> cky grammar sentence = unaryPass (foldl applyBinaryRule initTable [(i, j, k) | l <- [2..n], i <- [1..n-l+1], j <- [i..i+l-1], k <- [i..j-1]])
>   where
>     n = length sentence
>     initTable = M.fromList [((i, i), nonTerminalsFor word) | (i, word) <- zip [1..] sentence]
>     nonTerminalsFor word = [a | Unary a b <- grammar, b == word]
>     applyBinaryRule table (i, j, k) = foldl' (addRule i j k) table binaryRules
>       where
>         binaryRules = [rule | rule@(Binary _ b c) <- grammar, b `elem` (M.findWithDefault [] (i, k) table), c `elem` (M.findWithDefault [] (k+1, j) table)]
>     addRule i j k table (Binary a _ _) = M.insertWith (++) (i, j) [a] table
>     unaryPass table = foldl' applyUnaryRule table [(i, j) | i <- [1..n], j <- [i..n]]
>     applyUnaryRule table (i, j) = foldl' (addUnaryRule i j) table unaryRules
>       where
>         unaryRules = [rule | rule@(UnaryRule a b) <- grammar, b `elem` (M.findWithDefault [] (i, j) table)]
>     addUnaryRule i j table (UnaryRule a _) = M.insertWith (++) (i, j) [a] table


The main function has a very simple grammar, not fully utilized, 
and passes a sentence to be parsed using the cky function with the grammar, 
I attempted to make a unaryRule to solve the issue of optionality in binary rules,
(i.e. VP -> VP (NP)) but I don't think it quite worked in the cky function, 
I utilize all Unary and Binary rules to parse and print out the table 
and if it is a valid parse

more rules and sentences can be easily added by hand and handled by the cky function

I could not quite implement visual trees like in Python 


> main :: IO ()
> main = do
>   let grammar = [ Binary "S" "NP" "VP"
>                 , Binary "S" "VP" "NP"
>                 , Binary "S" "NP" "V"
>                 , Unary "D" "the"
>                 , Unary "D" "a"
>                 , Unary "P" "to"
>                 , Unary "P" "on"
>                 , Unary "N" "man"
>                 , Unary "N" "woman"
>                 , Unary "N" "Bob"
>                 , Unary "N" "dog"
>                 , Unary "N" "home"
>                 , Unary "V" "ran"
>                 , Unary "V" "jumps"
>                 , Unary "ADJ" "big"
>                 , Unary "ADJ" "small"
>                 , Binary "NP" "D" "N"
>                 , Binary "NP" "D" "NP"
>                 , Binary "NP" "ADJ" "N"
>                 , UnaryRule "VP" "V"
>                 , Binary "VP" "V" "NP"
>                 , Binary "PP" "P" "N"
>                 , Binary "VP" "V" "PP"
>                 , Binary "VP" "V" "N"
>                 ]

>   let sentences = ["the man ran home", "a big dog jumps on Bob", "the small woman ran", "a man jumps"]
>   forM_ sentences $ \sentence -> do
>     putStrLn $ "Sentence: " ++ show sentence
>     let tokens = T.words $ T.pack sentence
>     let table = cky grammar tokens
>     let n = length tokens
>     let isSentence = "S" `elem` (M.findWithDefault [] (1, n) table)
>     putStrLn $ "Is it a valid sentence? " ++ show isSentence
>     forM_ (M.toList table) $ \((i, j), nonTerminals) ->
>       putStrLn $ "NonTerminals at (" ++ show i ++ ", " ++ show j ++ "): " ++ show nonTerminals
>     putStrLn "" -- Add an empty line for readability
