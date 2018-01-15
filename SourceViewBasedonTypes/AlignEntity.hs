{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, ScopedTypeVariables, ViewPatterns #-}
module AlignEntity where
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List
import Data.List
import Data.Maybe
import GHC.Generics
import Debug.Trace
import BigraphTypes
import System.IO  
import Control.Monad
import Text.Regex
{-
nodesInCyPhyE :: [NodeType]
nodesInCyPhyE = ["AP","Light"]
-}
isNodesWithNeeds :: [NodeType]->[Tree]->Bool
isNodesWithNeeds ns nodes | length nodes == 0 = False
                          | isNodeWithNeeds ns (head nodes) == True = True
                          | otherwise = isNodesWithNeeds ns (tail nodes)

isNodeWithNeeds :: [NodeType]->Tree->Bool
isNodeWithNeeds ns node | paraType node `elem` ns = True
                        | isNodesWithNeeds ns (paraChildren node) == True = True
                        | otherwise = False

delOnePort :: String->[String]->[String]
delOnePort str strs | length strs == 0 = []
                    | head strs == str = tail strs
                    | otherwise = (head strs) : delOnePort str (tail strs)

isPortEqual :: [String]->[String]->Bool
isPortEqual str1 str2 | length str1==0 && length str2 == 0 = True
                      | length str1 == 0 || length str2 == 0 = False
                      | (head str1) `elem` str2 = isPortEqual (tail str1) (delOnePort (head str1) str2)
                      | otherwise = False

isThatNode :: [String]->Tree->Tree->Bool
isThatNode ns n1 n2 | paraType n1 == paraType n2 && isPortEqual(paraPort n1)(paraPort n2) = True
                    | paraType n1 == paraType n2 && paraType n1 `elem` ns = True
                    | otherwise = False

updateNodesWithoutNeeds :: [NodeType]->[Tree]->[Tree]
updateNodesWithoutNeeds ns nodes | length nodes == 0 = []
                                 | updateNodeWithoutNeeds ns (head nodes) == Null = updateNodesWithoutNeeds ns (tail nodes)
                                 | otherwise = updateNodeWithoutNeeds ns (head nodes): updateNodesWithoutNeeds ns (tail nodes)

updateNodeWithoutNeeds :: [NodeType]->Tree->Tree
updateNodeWithoutNeeds ns node |  paraType node `elem` ns = Null
                               | otherwise = Node (paraType node) (paraPort node) children
                                  where children = updateNodesWithoutNeeds ns (paraChildren node)


entity_sTsAlign ::[NodeType]-> BiGUL Tree Tree
entity_sTsAlign ns = $(update [p| Node x y z |] [p| Node x y z |] [d| x = Replace; y = Replace; 
                                                                      z = entity_mTmAlign ns |])


entity_mTmAlign :: [NodeType] -> BiGUL [Tree] [Tree]
entity_mTmAlign ns = Case [
       $(normalSV [p| [] |] [p| [] |] [p| [] |] )
              ==> Replace
      ,$(normal [| \ (s1:ss) (v1:vs) -> not (isNodeWithNeeds ns v1) |] [| const False |])
              ==> (Fail "view should be legal with nodeTypes in need")
      ,$(normal [| \ (s1:ss) (v1:vs) ->  isThatNode ns s1 v1 |][| \ (s1:ss)-> isNodeWithNeeds ns s1 |] )
              ==> $(update [p| x:xs |] [p| x:xs |] [d| x = entity_sTsAlign ns; xs = entity_mTmAlign ns |] )
      ,$(normal [| \(s1:ss) vs -> not (isNodeWithNeeds ns s1) |] [| \ (s1:ss)->not (isNodeWithNeeds ns s1) |])
         ==> $(update [p| _:ss  |] [p| ss |] [d| ss = entity_mTmAlign ns |] )
      ,$(adaptive [| \ss vv ->  isNodesWithNeeds ns ss |] )
              ==> (\ ss vv ->  updateNodesWithoutNeeds ns ss)
      ,$(adaptive [| \ ss (v1:vs) -> length ss == 0 |] )
              ==> (\ss (v1:vv) -> v1:ss)  
                     ]



{-
phyE_sTsAlign ::[NodeType]-> BiGUL Tree Tree
phyE_sTsAlign ns = 
         $(update [p| Node x y z |] [p| Node x y z |] [d| x = Replace; y = Replace; z = phyE_mTmAlign ns |])


phyE_mTmAlign :: [NodeType] -> BiGUL [Tree] [Tree]
phyE_mTmAlign ns = align
                     -- identify item contains view
                     (\ s1 ->  (isNodeWithNeeds ns s1) )  -- Node contains subtrees with NodeType in ns 
                     -- match between source and view
                     (\ s1 v1 -> isThatNode ns s1 v1 )  -- Same NodeType & NodePort or Same NodeType in ns
                     -- Update function
                     (phyE_sTsAlign ns)          
                     -- Create an item in the source from an item in the view
                     (\ v -> v)
                     -- update with source by deleting the contents not in view
                     (\ s1 -> case updateNodeWithoutNeeds ns s1 of { Null -> Nothing; x -> Just x }) 
                                                        --delete node with NodeType in ns
-}
{-

nS::[Tree]
nS = [Node "SO" [] [Node "Room" ["reception"] [Node "Door" ["d4"] [],Node "Door" ["d1"] [],Node "Agent" ["tom"] [],Node "Agent" ["carmen"] []],Node "Room" ["corridor"] [Node "Door" ["d1"] [],Node "Door" ["d3"] [],Node "Door" ["d5"] []],Node "Room" ["electricalroom"] [Node "Door" ["d4"] []],Node "Room" ["server"] [Node "Door" ["d3"] []],Node "Room" ["office"] [Node "Door" ["d5"] [],Node "Heater" ["off"] []]]]


nV :: [Tree]
nV = [Node "SO" [] [Node "Room" ["reception"] [Node "AP" ["wlan"] []],Node "Room" ["corridor"] [Node "Light" ["off"] [],Node "wlan]" [] []],Node "Room" ["server"] [Node "Light" ["on"] [],Node "wlan]" [] []],Node "Room" ["office"] [Node "Light" ["off"] [],Node "wlan]" [] []]]]


nSS :: [Tree]
nSS = [Node "Door" ["d1"] []]

nSS2 :: [Tree]
nSS2 = [Node "Door" ["d1"] [],Node "Agent" ["tom"] []]


nVV :: [Tree]
nVV = [Node "Agent" ["tom"] []]

-}






