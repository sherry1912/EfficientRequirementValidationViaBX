{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module EntityTypes where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Data.List
import Control.Monad.Except
import BigraphTypes
import Data.Maybe
import qualified Data.Map as Map
import LinkRelationTypes
import Data.List
-- General datatypes for relational data
type Entity = (Path,[Tree])
type Entities = [Entity]

addPathForEntities :: Path -> [Tree] -> [Tree]
addPathForEntities p nodes | length nodes == 0 = []
                           | otherwise = addPathForEntity p (head nodes) : addPathForEntities p (tail nodes)

addPathForEntity :: Path -> Tree -> Tree
addPathForEntity p n | paraRNChild p /= RNull = Node (paraRNType p) (paraRNPort p) [addPathForEntity (paraRNChild p) n]
                     | otherwise = Node (paraRNType p) (paraRNPort p) [n]

unpackageEntities :: Entities -> [Tree]
unpackageEntities rns | length rns == 0 = []
                      | otherwise = addPathForEntities (fst rn1) (snd rn1) ++  unpackageEntities (tail rns)
                          where rn1 = head rns

packageEntities :: [String] -> [Tree] -> Entities
packageEntities ns nodes  | length nodes == 0 = []
                          | otherwise = (pth, delPathForNodes pth n1):packageEntities ns n2
                            where pth = getRootPath ns (head nodes)
                                  (n1,n2) = seperateEntitiesByPath pth nodes

getRootPath :: [String] -> Tree  -> Path
getRootPath ns node | not (typ `elem` ns) && length children == 1 = Path typ port (getRootPath ns (head children))
                    | otherwise = RNull
                        where typ  = paraType node
                              port = paraPort node
                              children = paraChildren node 


delPathForNodes :: Path -> [Tree] -> [Tree]
delPathForNodes pth nodes | length nodes == 0 = []
                          | otherwise = delPathForNode pth (head nodes) ++ delPathForNodes pth (tail nodes)

delPathForNode :: Path -> Tree -> [Tree]
delPathForNode pth node | paraRNChild pth /= RNull = delPathForNode (paraRNChild pth) (head (paraChildren node))
                        | otherwise = (paraChildren node)

seperateEntitiesByPath :: Path->[Tree]->([Tree],[Tree])
seperateEntitiesByPath pth nodes | length nodes == 0 = ([],[])
                                 | isPathInside pth (head nodes)== True = (head nodes : n1, n2)
                                 | otherwise = (n1,head nodes : n2)
                              where (n1,n2) = seperateEntitiesByPath pth (tail nodes)
                                    


isPathInside :: Path -> Tree -> Bool
isPathInside pth node | pth == RNull = True
                      | paraRNType pth == paraType node && paraRNPort pth == paraPort node = isPathInside (paraRNChild pth) (head (paraChildren node))
                      | otherwise = False
                    

findmatchedbracket::String -> Int -> Int
findmatchedbracket str i | length str == 0 = error "given string is not legal when matching pair bracket"
                         | i==1 && (take 1 str) == ")" = 1
                         | i/=1 && (take 1 str) == ")" = 1 + findmatchedbracket (drop 1 str) (i-1)
                         | (take 1 str == "(") = 1 + findmatchedbracket (drop 1 str) (i+1)
                         | otherwise = 1 + findmatchedbracket (drop 1 str) i

