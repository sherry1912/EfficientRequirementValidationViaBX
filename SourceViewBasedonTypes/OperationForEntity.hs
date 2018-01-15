{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module OperationForEntity where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Data.List
import AlignEntity
import BigraphTypes
import EntityTypes
import EntityFormat
import BigraphFormat
import Data.Maybe
import qualified Data.Map as Map
import Data.List
import System.IO  
import Control.Monad
import Text.Regex
import Debug.Trace


judgeTypeAndPort :: Int->String->[String]->Tree->Bool
judgeTypeAndPort i str1 strs2 node | i>1 && length children == 1 = judgeTypeAndPort (i-1) str1 strs2 (head children)
                                   | i==1 && paraType node == str1 && strs2 /= [] && isPortEqual (paraPort node) strs2==True 
                                          = True
                                   | i==1 && strs2 == [] && paraType node == str1 = True
                                   | otherwise = False
                                   where children = paraChildren node


extractNodesForOrder :: Int->String->[String]->[Tree]->([Tree],[Tree])
extractNodesForOrder i str1 str2 nodes | length nodes == 0 = ([],[])
                                       | judgeTypeAndPort i str1 str2 (head nodes) == True =  ((head nodes):rn1,rn2)
                                       | otherwise =  (rn1,(head nodes):rn2)
                                          where (rn1,rn2) = extractNodesForOrder i str1 str2 (tail nodes)


orderNodesForOrder :: Int->[String]->[Tree]->[Tree]->[Tree]
orderNodesForOrder i ns nBIM nodes 
                           | length nodes == 0 = []
                           | length nBIM == 0 = nodes
              --             | not (paraType (head nBIM) `elem` ns) =  orderNodesForOrder i ns (tail nBIM) nodes 
                           | otherwise =  orderNodesForOrder (i+1) ns child rn1 ++ orderNodesForOrder i ns (tail nBIM) rn2
                             where (rn1,rn2) = extractNodesForOrder i typs port nodes
                                   typs = paraType (head nBIM)
                                   child = paraChildren (head nBIM)
                                   port = paraPort (head nBIM)

mergeHeadTwo :: [Tree]->[Tree]
mergeHeadTwo nodes | typ1==typ2 && port1==port2 = [Node typ1 port1 (mergeNodes (paraChildren n1 ++ paraChildren n2))]
                   | otherwise = nodes  
                     where typ1 = paraType n1
                           typ2 = paraType n2
                           port1 = paraPort n1
                           port2 = paraPort n2
                           n1 = head nodes
                           n2 = last nodes

mergeNodes :: [Tree]->[Tree]
mergeNodes nodes | length nodes == 0 = []
                 | length nodes == 1 = nodes
                 | length mergednode == 1 = mergeNodes (mergednode++(drop 2 nodes))
                 | otherwise = head nodes : mergeNodes (tail nodes)
                   where mergednode = mergeHeadTwo (take 2 nodes)


addPathForChild :: Tree->Tree->Tree
addPathForChild path node | path == Null = error "addPathForChild mistake<1>"
                          | length (paraChildren path) == 0 = Node (paraType path) (paraPort path) [node]
                          | otherwise = Node (paraType path) (paraPort path) [addPathForChild (head (paraChildren path)) node]

addPathForChildren :: Tree->[Tree]->[Tree]
addPathForChildren path nodes | length nodes == 0 = []
                              | otherwise = addPathForChild path (head nodes) : addPathForChildren path (tail nodes)

getIthType :: Int->Tree->String
getIthType i node | node == Null = error "getIthType mistake<1>"
                  | i ==1 = paraType node
                  | length (paraChildren node) /= 1 = error "getIthType mistake<2>" 
                  | otherwise = getIthType (i-1) (head (paraChildren node)) 

getIthPath :: Int->Tree->Tree
getIthPath i node | node == Null = error "getIthPath mistake<1>"
                  | i == 1 = Node (paraType node) (paraPort node) []
                  | length (paraChildren node) /=1 = error "getIthPath mistake<2>"
                  | otherwise = Node (paraType node) (paraPort node) [getIthPath (i-1) (head (paraChildren node))]

getIthChildren :: Int->Tree->[Tree]
getIthChildren i node | node == Null = error "getIthChildren mistake<1>"
                      | i == 1 = paraChildren node
                      | length (paraChildren node) /=1 = error "getIthChildren mistake<2>"
                      | otherwise = getIthChildren (i-1) (head (paraChildren node))


unmergeNode :: Int-> [String]->Tree->[Tree]
unmergeNode i ns node | not ((getIthType i node) `elem` ns) = unmergeNodes (i+1) ns nodesnew
                      | otherwise = [node]
                      where nodesnew = addPathForChildren path children
                            children = getIthChildren i node
                            path = getIthPath i node


unmergeNodes :: Int->[String]->[Tree]->[Tree]
unmergeNodes i ns nodes | length nodes == 0 = []
                        | otherwise = unmergeNode i ns (head nodes) ++ unmergeNodes i ns (tail nodes)

