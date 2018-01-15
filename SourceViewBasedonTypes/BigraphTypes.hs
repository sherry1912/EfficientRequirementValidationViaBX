{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module BigraphTypes where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Data.List
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map as Map
import Data.List
-- General datatypes for relational data

data Tree = Node NodeType NodePort Children | Null
                                          deriving(Show, Eq)
type NodeType = String
type NodePort = [String]
type Children = [Tree]
deriveBiGULGeneric ''Tree

data BiGraph = BiGraph Tree deriving(Show, Eq)
deriveBiGULGeneric ''BiGraph

type BiGraphs = [BiGraph] 

paraType :: Tree -> String
paraType (Node nodetype _ _ ) = nodetype

paraPort :: Tree -> [String]
paraPort (Node _ nodeport _ ) = nodeport

paraChildren :: Tree -> [Tree]
paraChildren (Node _ _ children) = children

paraBType :: BiGraph -> String
paraBType (BiGraph (Node nodetype _ _ )) = nodetype

paraBPort :: BiGraph -> [String]
paraBPort (BiGraph (Node _ nodeport _ )) = nodeport

paraBChildren :: BiGraph -> [Tree]
paraBChildren (BiGraph (Node _ _ children)) = children

paraBNode :: BiGraph -> Tree
paraBNode (BiGraph node) = node

unpackageBigraphs :: [BiGraph] -> [Tree]
unpackageBigraphs bis | length bis == 0 = []
                      | otherwise = paraBNode (head bis) : unpackageBigraphs (tail bis)

packageBigraphs :: [Tree] -> [BiGraph]
packageBigraphs ns | length ns == 0 = []
                   | otherwise = BiGraph (head ns) : packageBigraphs (tail ns)



