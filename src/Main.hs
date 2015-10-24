-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Tsushko, 2015
-- License     :  BSD3
--
-- Maintainer  :  artem.tsushko@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Graph
import AllSimplePaths
import System.Environment (getArgs)

main :: IO ()
main = do
    (filename : start : target : _) <- getArgs
    contents <- readFile filename
    let graph = read contents :: Graph Int
        paths = findAllSimplePaths
                    graph
                    (mkNode . read $ start)
                    (mkNode . read $ target)
    putStr $ showPaths paths
    return ()
