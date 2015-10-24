-----------------------------------------------------------------------------
-- |
-- Module      :  AllSimplePaths
-- Copyright   :  (c) Artem Tsushko, 2015
-- License     :  BSD3
--
-- Maintainer  :  artem.tsushko@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
--
-----------------------------------------------------------------------------

module AllSimplePaths (
    Path,
    findAllSimplePaths,
    showPaths,
    showPath
) where

import Graph
import Control.Monad (forM_)

type Path a = [Edge a]

-- | Finds all simple paths between start and target nodes
findAllSimplePaths  :: (Eq a) => Graph a -- ^ the connected graph
                    -> Node a -- ^ start Node
                    -> Node a -- ^ target node
                    -- | all simple paths between start and target nodes
                    -> [Path a]
findAllSimplePaths graph start target =
    allSimplePaths graph target start [] []

allSimplePaths  :: (Eq a) => Graph a -- ^ the connected graph
                -> Node a -- ^ target Node
                -> Node a -- ^ start node
                -> [Node a] -- ^ already visited nodes
                -> Path a -- ^ path to the current node
                -- | all simple paths between start and target nodes
                -> [Path a]
allSimplePaths graph target start visited path
    | target == start = [path]
    | otherwise =
        let nextToVisit = filter (`notElem` visited) $ neighbourhood graph start
            nextVisited = start : visited
            nextPath node = path ++ [mkEdge start node]
            pathsToTarget node = allSimplePaths graph target node nextVisited
                                 $ nextPath node
        in concatMap pathsToTarget nextToVisit

showPath :: (Show a) => Path a -> String
showPath path =
    if null path then "" else (show . fromNode . firstNode . head $ path)
    ++ concatMap ((" " ++) . show . fromNode . secondNode) path

showPaths :: (Show a) => [Path a] -> String
showPaths paths = unlines $ map showPath paths







