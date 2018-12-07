module Graph
  ( Graph(Graph)
  , removeEdge
  , outbound
  , inbound
  , tsort
  ) where

import Data.List ((\\), minimum, sort)

-- from http://5outh.blogspot.com/2012/12/graphs-and-topological-sorting-in.html
data Graph a = Graph
  { vertices :: [a]
  , edges :: [(a, a)]
  } deriving (Show)

removeEdge :: (Eq a) => (a, a) -> Graph a -> Graph a
removeEdge x (Graph v e) = Graph v (filter (/= x) e)

connections :: (Eq a) => ((a, a) -> a) -> a -> Graph a -> [(a, a)]
connections f x (Graph _ e) = filter ((== x) . f) e

--outbound connections
outbound :: Eq b => b -> Graph b -> [(b, b)]
outbound = connections fst

--inbound connections
inbound :: Eq b => b -> Graph b -> [(b, b)]
inbound = connections snd

tsort :: (Eq a, Ord a) => Graph a -> [a]
tsort graph = tsort' [] (noInbound graph) graph
  where
    noInbound (Graph v e) = sort $ filter (flip notElem $ map snd e) v
    tsort' l [] (Graph _ []) = reverse l
    tsort' _ [] _ = error "There is at least one cycle in this graph."
    tsort' l v g = tsort' (v' : l) s' g'
      where
        outEdges = outbound v' g
        outVerts = map snd outEdges
        v' = minimum v
        g' = foldr removeEdge g outEdges
        s' = (v \\ [v']) ++ filter (null . flip inbound g') outVerts
