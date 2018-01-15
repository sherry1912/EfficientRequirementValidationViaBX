{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module EntityFormat where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Debug.Trace
import Data.List
import EntityTypes
import BigraphTypes
import LinkRelationTypes
import Data.Maybe
import qualified Data.Map as Map
import Data.List
import System.IO  
import Control.Monad
import Text.Regex

paserEntities :: String->Entities
paserEntities str | length str == 0 = []
                  | otherwise = (path,nodes) : paserEntities strremain
                    where x = findmatchedbracket str 0
                          strlr =  drop 1 (take (x-1) str)
                          strremain = drop x str
                          xx =  findfirstlegalcomma str True
                    --      strss = splitRegex (mkRegex ",") strlr
                          path = paserRNode (take (xx-1) strlr)
                          nodes = paserStrsNodes (splitbycomma (drop (xx-1) strlr))

splitbycomma :: String -> [String]
splitbycomma str | length str == 0 = []
                 | otherwise = take (x-1) str : splitbycomma (drop x str)
                    where x = findfirstlegalcomma str True

paserStrsNodes :: [String]->[Tree]
paserStrsNodes strs | length strs == 0 = []
                    | otherwise = paserNode (head strs) : paserStrsNodes (tail strs)

paserNode :: String -> Tree
paserNode str = Node nodetype nodeport nodechildren
                where nodetype = takeWhile (/='[') (takeWhile (/='.') str)
                      portstr = takeWhile (/=']') (drop 1 (dropWhile (/='[')(takeWhile (/='.') str)))
                      nodeport =  splitRegex (mkRegex ",") portstr
                      nodechildren = paserNodes (drop 1 (dropWhile (/='.') str))

paserNodes :: String -> [Tree]
paserNodes str | length str == 0 = []
               | (take 1 str) == "(" = paserNodes (drop 1 (take ( (findmatchedbracket str 0)-1) str))
               | otherwise = paserNode (take (x-1) str) : paserNodes (drop x str)
                   where x = findfirstmatchedseparator str


paserRNode :: String->Path
paserRNode str | length str == 0 = RNull
               | otherwise = Path p1 pp2 (paserRNode childstr) 
                 where p1 = takeWhile (/='[') (takeWhile (/='.') str)
                       p2 = takeWhile (/=']') (drop 1 (dropWhile (/='[') (takeWhile (/='.') str)))
                       pp2 = if p2== "" then [] else [p2]
                       childstr = drop 1 (dropWhile (/='.') str)


showEntities :: [Entity] -> String
showEntities lrs | length lrs == 0 = ""
                 | otherwise = showEntity (head lrs) ++ showEntities (tail lrs)

showEntity :: (Path,[Tree]) -> String
showEntity lr = "( "++ path ++ nodes ++ " )\n"
                  where path = showRNode (fst lr)
                        nodes = showNodesFst (snd lr)

findfirstmatchedseparator :: String -> Int
findfirstmatchedseparator str | length str == 0 = 1
                              | (take 1 str) == "|" = 1
                              | (take 1 str) == "(" = x + findfirstmatchedseparator (drop x str) 
                              | otherwise = 1 + findfirstmatchedseparator (drop 1 str)
                                  where x =  findmatchedbracket str 0

findfirstlegalcomma :: String ->Bool-> Int
findfirstlegalcomma str b | length str == 0 = 1
                          | (take 1 str) == "[" = 1 + findfirstlegalcomma (drop 1 str) False
                          | (take 1 str) == "]" = 1 + findfirstlegalcomma (drop 1 str) True 
                          | (take 1 str) == "," && b == True = 1
                          | otherwise = 1 + findfirstlegalcomma (drop 1 str) b
                                  


showRNode :: Path -> String
showRNode rn | rn == RNull = ""
             | otherwise = (paraRNType rn) ++ 
                           (showPorts port)++
                           (if child==RNull then "" else "."++showRNode child)
                           where port = paraRNPort rn
                                 child = paraRNChild rn

showPortsnd :: [String]->String
showPortsnd strs = if length strs == 0 then ""
                else ","++ head strs ++showPortsnd (tail strs)

showPorts ::[String]->String
showPorts strs | length strs == 0 = ""
               | length strs == 1 = "["++head strs++"]"
               | otherwise = "["++head strs++showPortsnd (tail strs)++"]"

showNode :: Tree -> String
showNode node | length children == 0 = str0
              | otherwise = str0 ++ ".(" ++ showNodesSnd children False ++ ")"
                where nodetype = paraType node
                      port = paraPort node
                      children = paraChildren node 
                      str0 = nodetype ++ showPorts port

showNodesFst :: [Tree]->String
showNodesFst nodes | length nodes == 0 = ""
                   | otherwise = " , "++showNode (head nodes) ++ showNodesFst (tail nodes)

showNodesSnd :: [Tree]->Bool->String
showNodesSnd children b | length children == 0 = ""
                        | b == False = ""++(showNode (head children))++(showNodesSnd (tail children) True)
                        | otherwise  = "|"++(showNode (head children))++(showNodesSnd (tail children) True)

