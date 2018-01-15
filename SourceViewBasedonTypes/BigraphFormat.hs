{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, ScopedTypeVariables #-}
module BigraphFormat where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Data.List
import Data.Maybe
import GHC.Generics
import Debug.Trace
import BigraphTypes
import System.IO  
import Control.Monad
import Text.Regex

test_BigraphFormat = do  
        handle <- openFile "cps.txt" ReadMode
        contents <- hGetContents handle
        let wordslist = words contents
            str = concat wordslist
        let bis = paserbigraphs str
        let trees = unpackageBigraphs bis
        let bisnew = packageBigraphs trees
        writeFile "result.txt" "this is the bigraphs:\n"
        appendFile "result.txt" (showBiGraphs bis)
        appendFile "result.txt" "\n\nthis is the bigraphs:\n"
        appendFile "result.txt" (showBiGraphs bisnew)
        hClose handle


findmatchedbracket::String -> Int -> Int
findmatchedbracket str i | length str == 0 = error "given string is not legal when matching pair bracket"
                         | i==1 && (take 1 str) == ")" = 1
                         | i/=1 && (take 1 str) == ")" = 1 + findmatchedbracket (drop 1 str) (i-1)
                         | (take 1 str == "(") = 1 + findmatchedbracket (drop 1 str) (i+1)
                         | otherwise = 1 + findmatchedbracket (drop 1 str) i

finddoubleseparator::String -> Int
finddoubleseparator str | length str == 0 = 0
                        | take 2 str == "||" = 0
                        | take 1 str == "|" && take 2 str /= "||" = 1 + finddoubleseparator (drop 1 str)
                        | otherwise = length (takeWhile (/='|') str) + finddoubleseparator (dropWhile (/='|') str)

paserbigraphs :: String  -> BiGraphs
paserbigraphs str   | length str == 0 = []
                    | otherwise = paserbigraph (take x str) : paserbigraphs (drop (x+2) str)
                       where x = finddoubleseparator str

paserbigraph :: String -> BiGraph
paserbigraph str = BiGraph (Node nodetype nodeport nodechildren)
                   where nodetype = takeWhile (/='[') (takeWhile (/='.') str)
                         portstr = takeWhile (/=']') (drop 1 (dropWhile (/='[')(takeWhile (/='.') str)))
                         nodeport = splitRegex (mkRegex ",") portstr
                         nodechildren = pasernodes (drop 1 (dropWhile (/='.') str))

pasernode :: String -> Tree
pasernode str = Node nodetype nodeport nodechildren
                where nodetype = takeWhile (/='[') (takeWhile (/='.') str)
                      portstr = takeWhile (/=']') (drop 1 (dropWhile (/='[')(takeWhile (/='.') str)))
                      nodeport = splitRegex (mkRegex ",") portstr
                      nodechildren = pasernodes (drop 1 (dropWhile (/='.') str))

pasernodes :: String -> [Tree]
pasernodes str | length str == 0 = []
               | (take 1 str) == "(" = pasernodes (drop 1 (take ( (findmatchedbracket str 0)-1) str))
               | otherwise = pasernode (take (x-1) str) : pasernodes (drop x str)
                   where x = findfirstmatchedseparator str

findfirstmatchedseparator :: String -> Int
findfirstmatchedseparator str | length str == 0 = 1
                              | (take 1 str) == "|" = 1
                              | (take 1 str) == "(" = x + findfirstmatchedseparator (drop x str) 
                              | otherwise = 1 + findfirstmatchedseparator (drop 1 str)
                                  where x =  findmatchedbracket str 0



showNode :: Tree -> String
showNode node | length children == 0 = str0
              | otherwise = str0 ++ ".(" ++ showNodes children False ++ ")"
                where nodetype = paraType node
                      nodeport = paraPort node
                      children = paraChildren node 
                      str0 = nodetype ++ showPorts nodeport

showPortsnd :: [String]->String
showPortsnd strs = if length strs == 0 then ""
                else ","++ head strs ++showPortsnd (tail strs)

showPorts ::[String]->String
showPorts strs | length strs == 0 = ""
               | length strs == 1 = "["++head strs++"]"
               | otherwise = "["++head strs++showPortsnd (tail strs)++"]"

showNodes :: [Tree]->Bool->String
showNodes children b | length children == 0 = ""
                                 | b == False = ""++(showNode (head children))++(showNodes (tail children) True)
                                 | otherwise  = "|"++(showNode (head children))++(showNodes (tail children) True)


showNodesInfirstLevel :: [Tree]->Bool->String
showNodesInfirstLevel children b | length children == 0 = ""
                                 | b == False = "     "++(showNode (head children))++"\n"++(showNodesInfirstLevel (tail children) True)
                                 | otherwise  = "  |  "++(showNode (head children))++"\n"++(showNodesInfirstLevel (tail children) True)


showBiGraph :: BiGraph -> String
showBiGraph g | length children == 0 = str0
              | otherwise = str0 ++ ".(\n" ++ showNodesInfirstLevel children False ++ ")\n"
                where nodetype = paraBType g
                      nodeport = paraBPort g
                      children = paraBChildren g 
                      str0 = nodetype ++ showPorts nodeport

showBiGraphs::BiGraphs -> String
showBiGraphs gs | length gs == 0 = ""
                | otherwise = showBiGraph (head gs) ++ showBiGraphs (tail gs)



