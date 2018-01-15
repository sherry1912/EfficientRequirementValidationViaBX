{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module BXFamily where

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
import BXCyberEntity
import BXCyberPhysicalEntity
import BXPhysicalEntity
import BXRequirement1
import BXRequirement2


get_data_Entity = do
        handle <- openFile "CPS.txt" ReadMode
        contents <- hGetContents handle
        let wordslist = words contents
            str = concat wordslist
        let bis = paserbigraphs str
        let nodes = unpackageBigraphs bis 
        handle <- openFile "cyberphysicalEntity.txt" ReadMode
        contents <- hGetContents handle
        let wordslist = words contents
            str = concat wordslist
        let lrs = paserEntities str
        let entities = unpackageEntities lrs
        let entitiess = orderNodesForOrder 1 nodesInPhyE nodes entities
        let entities1 = mergeNodes entitiess
        print entities1     
        writeFile "result.txt" "\nthis is the source:\n"
        appendFile "result.txt" (show nodes)
        appendFile "result.txt" "\nthis is the view:\n"
        appendFile "result.txt" (show entities1)
        hClose handle

