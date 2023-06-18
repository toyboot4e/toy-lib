{-# LANGUAGE BangPatterns #-}

module Data.Graph.Digraph where

import Data.Array.IArray
import Data.Bifunctor
import Data.Graph (Graph, Vertex)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')

-- {{{ Digraph

-- | Red | Green color
type Color = Bool

-- | Colored vertices in a bipartite graph
type ColorInfo = ([Int], [Int])

-- | DFS with vertices given colors
colorize :: Graph Int -> IM.IntMap Color -> Vertex -> (IM.IntMap Color, Maybe ColorInfo)
colorize !graph !colors0 = dfs True (colors0, Just ([], []))
  where
    dfs :: Color -> (IM.IntMap Color, Maybe ColorInfo) -> Vertex -> (IM.IntMap Color, Maybe ColorInfo)
    dfs !color (!colors, !acc) !v =
      let (!colors', !acc') = setColor color (colors, acc) v
       in if IM.member v colors
            then (colors', acc')
            else foldl' (dfs (not color)) (colors', acc') $ graph ! v

    setColor :: Color -> (IM.IntMap Color, Maybe ColorInfo) -> Vertex -> (IM.IntMap Color, Maybe ColorInfo)
    setColor !color (!colors, !acc) !v =
      case IM.lookup v colors of
        Just c
          | c == color -> (colors, acc)
          | otherwise -> (colors, Nothing)
        Nothing -> (IM.insert v color colors, applyColor color v acc)

    applyColor :: Color -> Vertex -> Maybe ColorInfo -> Maybe ColorInfo
    applyColor !_ !_ Nothing = Nothing
    applyColor !color !v (Just !acc)
      | color = Just $ first (v :) acc
      | otherwise = Just $ second (v :) acc

-- }}}
