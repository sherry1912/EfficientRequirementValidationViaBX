{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module BXRequirement1 where

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
import OperationForEntity

nodesInR1 :: [NodeType]
nodesInR1 = ["Room", "Light", "Brightness","Agent"]

main_get_requirement1 = do  
        handle <- openFile "CPS.txt" ReadMode
        contents <- hGetContents handle
        let wordslist = words contents
            str = concat wordslist
        let bis = paserbigraphs str
        let nodes = unpackageBigraphs bis
        let nodes1 = fromJust $ get (entity_mTmAlign nodesInR1) nodes
        let biv = packageBigraphs nodes1
        writeFile "result.txt" "this is the cyber physical space :\n"
        appendFile "result.txt" (showBiGraphs bis)     
        appendFile "result.txt" "\nthis is the requirement 1 view:\n"
        appendFile "result.txt" (showBiGraphs biv)
        writeFile "requirement1.txt" (showBiGraphs biv)
        hClose handle



main_put_requirement1 = do  
        handle <- openFile "CPS.txt" ReadMode
        contents <- hGetContents handle
        let wordslist = words contents
            str = concat wordslist
        let bis = paserbigraphs str
        let nodes = unpackageBigraphs bis
        handle <- openFile "requirement1.txt" ReadMode
        contents <- hGetContents handle
        let wordslist = words contents
            str = concat wordslist
        let biv = paserbigraphs str
        let bivv = unpackageBigraphs biv
        let nodes_new = fromJust $ put (entity_mTmAlign nodesInR1) nodes bivv
        let nodes_new_pac = packageBigraphs nodes_new
        writeFile "result.txt" "this is the original cyber physical space:\n"
        appendFile "result.txt" (showBiGraphs bis)     
        appendFile "result.txt" "\nthis is the requirement 1 view:\n"
        appendFile "result.txt" (showBiGraphs biv)
        appendFile "result.txt" "\nthis is the updated cyber physical space:\n"
        appendFile "result.txt" (showBiGraphs nodes_new_pac)
        writeFile "CPS.txt" (showBiGraphs nodes_new_pac)
        hClose handle



