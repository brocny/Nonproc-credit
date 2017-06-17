data Node a = Node { num :: Int, height :: Int, excess :: a }

data Edge a = Edge { from :: Node a, to :: Node a, capacity :: a, reserve :: a }

data Graph a = Graph { src :: Node a, snk :: Node a, nodes :: [Node a], edges :: [Edge a]}

node1 = Node 1 0 0
node2 = Node 2 0 0 
node3 = Node 3 0 0 
sink = Node 4 0 0
source = Node 5 0 0 

gr = Graph source sink [source,sink,node1,node2,node3] [Edge source node1 5 5, Edge source node2 4 4, Edge node2 node1 6 6, Edge node1 node3 7 7, Edge node2 sink 3 3, Edge node3 sink 7 7]

initializePreFlow :: (Num a) => Graph a -> Graph a
initializePreFlow (Graph (Node n h e) si ns es) = (Graph (Node n (length ns) 10000000) si (map (\(Node n l e) -> (Node n 0 0)) ns) (map (\(Edge f t c r) -> (Edge f t c c)) es)) 
{-
transferFlowFromSource :: (Num a) => Graph a -> Graph a
transferFlowFromSource (Graph so si ns es) = 
     
-}

nodesConnectedToSource :: Graph a -> [Node a]
nodesConnectedToSource gr = map to (edgesFromSource gr)


edgesFromSource :: Graph a -> [Edge a]
edgesFromSource (Graph sr sn ns es) = filter (\e -> (num $ from $ e) == (num sr)) es



{-
addReverseEdges :: (Num a) => [Edge a] -> [Edge a]
addReverseEdges [] = []
addReverseEdges ((Edge f t c r):edges) = if (any (\(Edge f1 t1 _ _)->(num f1 == num t && num t1 == num f)) edges) then (Edge f t c r) : addReverseEdges edges
                                                                                                  else (Edge f t c r) : (Edge t f 0 0) : addReverseEdges edges
-}

nodesWithExcess :: Graph a -> [Node a]
nodesWithExcess (Graph sr sn ns _) = filter (\(Node n h f) -> h>0 && n /= num sr && n /= num sn) ns
	
	
relabel :: Node a -> Node a
relabel (Node n h e) = (Node n (h + 1) e)

push :: (Num a, Ord a) => Graph a -> Edge a -> Graph a
push (Graph source sink nodes edges) edge@(Edge f t c r) = Graph source sink (((Node (num f) (height f) ((excess f) - delta))) : (Node (num t) (height t)  ((excess t) + delta)) : [n | n <- nodes, num n /= (num f) && num n /= (num t)]) ((Edge f t c (r - delta)):(filter (\e -> (num $ from $ e) /= (num f) ||(num $ to $ e) /= (num t)) edges))
    where delta = min (excess f) (reserve edge)
	
