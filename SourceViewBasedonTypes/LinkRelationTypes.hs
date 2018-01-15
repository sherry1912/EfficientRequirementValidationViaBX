{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module LinkRelationTypes where

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


data Path = Path NodeType NodePort Child | RNull
                              deriving(Show, Eq)
type LinkRName = String
type NodeType = String
type NodePort = [String]
type Child = Path
deriveBiGULGeneric ''Path

type LinkRelations = [(LinkRName,[Path])]



paraRNType :: Path -> String
paraRNType (Path nodetype _ _ ) = nodetype

paraRNPort :: Path -> [String]
paraRNPort (Path _ nodeport _ ) = nodeport

paraRNChild :: Path -> Path
paraRNChild (Path _ _ child ) = child



packageLRelations :: [Path]-> LinkRelations
packageLRelations rns | length rns == 0 = []
                      | otherwise = (port, delPort_rnodes n1):packageLRelations n2
                        where port = getLeafPort (head rns)
                              (n1,n2) = seperateLRelations port rns

unpackageLRelations :: LinkRelations -> [Path]
unpackageLRelations rns | length rns == 0 = []
                        | otherwise = addPort_rnodes (fst rn1) (snd rn1) ++  unpackageLRelations (tail rns)
                          where rn1 = head rns


getLeafPort :: Path -> String
getLeafPort rn | rn == RNull = ""
               | paraRNChild rn == RNull = head (paraRNPort rn)
               | otherwise = getLeafPort (paraRNChild rn)

seperateLRelations :: String->[Path]->([Path],[Path])
seperateLRelations str rns | length rns == 0 = ([],[])
                           | getLeafPort (head rns) == str = ((head rns):n1,n2)
                           | otherwise = (n1,(head rns):n2)
                             where (n1,n2)= seperateLRelations str (tail rns)

delPort_rnode :: Path->Path
delPort_rnode rn | rn == RNull = RNull
                 | p3 == RNull = Path p1 [] p3
                 | otherwise = Path p1 p2 (delPort_rnode p3)
                         where p1 = paraRNType rn
                               p2 = paraRNPort rn
                               p3 = paraRNChild rn

delPort_rnodes :: [Path]->[Path]
delPort_rnodes rns | length rns == 0 = []
                   | otherwise = delPort_rnode (head rns) : delPort_rnodes (tail rns)


addPort_rnode :: String->Path->Path
addPort_rnode str rn | p3 == RNull = Path p1 [str] p3
                     | otherwise = Path p1 p2 (addPort_rnode str p3)
                         where p1 = paraRNType rn
                               p2 = paraRNPort rn
                               p3 = paraRNChild rn

addPort_rnodes :: String->[Path]->[Path]
addPort_rnodes str rns | length rns == 0 = []
                       | otherwise = addPort_rnode str (head rns) : addPort_rnodes str (tail rns)


