-- Atur Lima 166916
-- Joao Pedro Martins 176117

import Data.List

data Vertex = Null | Vertex { key :: (String, String) -- key in form (id, plane)
                            , dist :: Float           -- shortest path distance
                            , pk :: (String, String)  -- parent key in shortest path
                            , marked :: Bool          -- for unmarked set in Dijkstra
                            , adj :: Adj              -- adjacency list
                            } deriving (Show)

data Adj = None | Adj { nkey :: (String, String) -- current neighbor key
                      , w :: Float               -- current edge weight
                      , adjNext :: Adj           -- next
                      } deriving (Show)

data Graph = Empty | Graph { graphV :: Vertex    -- current vertex
                           , graphNext :: Graph  -- next
                           } deriving (Show)

--------------------------------------------------------

-- Prepares input and split it in a 3-tuple
parse_input :: String -> ([[String]], [[String]], [String])
parse_input contents = (filter ((== 4). length) (map words . lines $ contents),init (filter ((== 2). length) (map words . lines $ contents)), last (map words . lines $ contents))


-- Inserts neighbor key nk in end of adjacency list received
adj_insert :: (String,String) -> Float -> Adj -> Adj
adj_insert nk w None = Adj nk w None
adj_insert nk w (Adj nk' w' next) = Adj nk' w' (adj_insert nk w next)


 -- Returns True if vertex with key k is in graph, False otherwise
in_graph :: (String,String) -> Graph -> Bool
in_graph k Empty = False
in_graph k (Graph v next)
    | key v == k = True
    | otherwise = in_graph k next


-- Make vertex with key k2 neighbor of vertex with key k1 with weight
-- w in graph received. Assumes existance of vertex with key k1 in graph
make_neighbor :: (String,String) -> (String,String) -> Float -> Graph -> Graph
make_neighbor k1 k2 w Empty = Empty
make_neighbor k1 k2 w (Graph v next)
    | k1 == (key v) = Graph (Vertex (key v) (dist v) (pk v) (marked v) (adj_insert k2 w (adj v))) next
    | otherwise = (Graph v (make_neighbor k1 k2 w next))


-- Returns vertex with key k in graph received.
-- Assumes existance of vertex with key k in graph
find_vertex :: (String,String) -> Graph -> Vertex
find_vertex k (Graph v next)
    | k == (key v) = v
    | otherwise = find_vertex k next


-- Inserts vertex v in graph received
graph_insert :: Vertex -> Graph -> Graph
graph_insert v Empty = Graph v Empty
graph_insert v (Graph u next)
    | (key v) == (key u) = Graph u next
    | otherwise = Graph u (graph_insert v next)


-- Get list of keys of all bus line vertexes
list_line_vxs :: Graph -> [(String, String)]
list_line_vxs g = list_line_vxs' g []
    where list_line_vxs' Empty acc = acc
          list_line_vxs' (Graph v next) acc
              | snd (key v) /= "a-pe" = list_line_vxs' next ((key v):acc)
              | otherwise = list_line_vxs' next acc


-- Build graph with given input with multiple connected components,
-- each containing vertexes of the same plane (i.e. vertexes v and u
-- are in the same component if and only if snd (key v) == snd (key u))
build_graph :: [[String]] -> Graph
build_graph input =
    let g = foldr (\v acc_g -> graph_insert v acc_g) Empty $ map (\k -> Vertex k (1.0/0.0) ("","") False None) $ nub $ concat $ map (\(x1:x2:pl:xs) -> [(x1,pl),(x2,pl)]) input
                    in foldr (\(k1,k2,w) -> make_neighbor k1 k2 w ) g $ concat
                       $ map (\[x1,x2,pl,w] -> [((x1,pl),(x2,pl),(read w :: Float))]) input


-- Creates edges with weights (bus-line waiting time)/2 from each vertex with
-- key (id,"a-pe") to all vertex with key (id,"linha-xxx") and edges with
-- weight 0 from all vertex with key (id,"linha-xxx") to each vertex with
-- key (id,"a-pe") in graph g
connect_graph :: Graph -> [[String]] -> Graph
connect_graph g busLineInput =
    let l = list_line_vxs g
        g' = foldr (\(k,pl,w) acc -> if (in_graph (k,"a-pe") g) then make_neighbor (k,"a-pe") (k,pl) (w / 2) acc else acc) g $ f l busLineInput []
    in  foldr (\(k,pl,w) acc -> if (in_graph (k,"a-pe") g) then make_neighbor (k,pl) (k,"a-pe") 0 acc else acc) g' $ f l busLineInput []
    where f [] ys acc = acc
          f ((x1,x2):xs) ys acc = f xs ys ([(x1,x2,(read yn :: Float))]++acc)
              where yn = last . last $ filter (\y -> head y == x2) ys


-- Set distance of vertex with key k to d in graph received
set_distance :: (String,String) -> Float -> Graph -> Graph
set_distance k d (Graph v next)
    | k == key v = Graph (Vertex (key v) (d) (pk v) (marked v) (adj v)) next
    | otherwise = Graph v (set_distance k d next)


-- Finds and returns unmarked vertex with min dist in graph received.
min_unmarked :: Graph -> Vertex
min_unmarked g = min_unmarked' g Null (1.0/0.0)
    where min_unmarked' Empty minV _ = minV
          min_unmarked' (Graph v next) minV minD
              | not (marked v) && dist v < minD =  min_unmarked' next v (dist v)
              | otherwise =  min_unmarked' next minV minD


-- Marks vertex with key k in graph received
mark :: (String,String) -> Graph -> Graph
mark k (Graph v next)
    | key v == k = Graph (Vertex (key v) (dist v) (pk v) (True) (adj v)) next
    | otherwise = Graph v (mark k next)


-- Get neighbors of vertex with adjacency list received
neighbors :: Adj -> Graph -> [(Vertex,Float)]
neighbors None g = []
neighbors (Adj k w next) g = let n = (find_vertex k g)
                             in (n,w):(neighbors next g)


-- Relax edge from vertex v to vertex u with weight w
relax :: Vertex -> Float -> Vertex -> Graph -> Graph
relax u w v (Graph v' next)
    | key v' == key u = if (w + dist v) <= (dist u) then Graph (Vertex (key u)
                                                                       (w + dist v)
                                                                       (key v)
                                                                       (marked u)
                                                                       (adj u)) next
                                                    else Graph (Vertex (key u)
                                                                       (dist u)
                                                                       (pk u)
                                                                       (marked u)
                                                                       (adj u)) next

    | otherwise = Graph v' (relax u w v next)


-- Set distance of initial vertex s to 0 in graph g
-- and pass them to dijstra'
dijkstra :: Graph -> (String,String) -> Graph
dijkstra g sk =
  let g' = set_distance sk 0 g
      s = find_vertex sk g'
  in  dijkstra' g' s


-- Dijkstra's algorithm (recursive).
-- Mark current vertex v as marked in graph.
-- Get its neighbors into list ns .
-- Relax edges from v to its neighbors.
-- Get unmarked vertex with min distance from graph.
-- The algorithm terminates when all nodes have been marked (so v is Null),
-- returning graph with updated parents and distances.
dijkstra' :: Graph -> Vertex -> Graph
dijkstra' g Null = g
dijkstra' g v =
  let g' = mark (key v) g
      ns = neighbors (adj v) g'
      g'' = foldr (\(n,w) -> relax n w v) g' ns
      v' = min_unmarked g''
  in dijkstra' g'' v'


-- Returns the minimum cost path from the initial vertex to vertex v.
pnew :: Vertex -> (String,String) -> Graph -> [(String, String)]
pnew v kPrev g
    | (pk v) == ("","") = if fst (kPrev) == fst (key v) then [] else [key v]
    | fst (kPrev) /= fst (key v) = (key v):(pnew (find_vertex (pk v) g) (key v) g)
    | otherwise = pnew (find_vertex (pk v) g) (key v) g


-- Get key of initial vertex of the path
initial :: [String] -> (String, String)
initial contents = (head $ contents, "a-pe")


-- Get key of final vertex of the path
final :: [String] -> (String, String)
final contents = (last $ contents, "a-pe")


main :: IO ()
main =  do
        input <- getContents
        let (graphInput, busLineInput, queryInput) = parse_input input
            g = build_graph graphInput
            g' = connect_graph g busLineInput
            ki = initial queryInput
            gf = dijkstra g' ki
            kf = final queryInput
            f = find_vertex kf gf
            path = putStrLn . unwords $ init $ concat $ map(\(x,y) -> [x,y]) $ reverse (pnew f ("","") gf)
        path
        print(dist f)
