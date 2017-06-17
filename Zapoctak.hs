data Node a = Node { num :: Int, height :: Int, flow :: a }

data Edge a = Edge { from :: Node a, to :: Node a, capacity :: a, reserve :: a }

data Graph a = Graph { src :: Node a, snk :: Node a, nodes :: [Node a], edges :: [Edge a]}

node1 = Node 1 0 0
node2 = Node 2 0 0 
node3 = Node 3 0 0 
sink = Node 4 0 0
source = Node 5 0 0 

gr = Graph source sink [source,sink,node1,node2,node3] [Edge source node1 5 5, Edge source node2 4 4, Edge node2 node1 6 6, Edge node1 node3 7 7, Edge node2 sink 3 3, Edge node3 sink 7 7]

initializePreFlow :: (Num a) => Graph a -> Graph a
initializePreFlow (Graph (Node n h e) si ns es) = (Graph (Node n (length ns) 10000) si (map (\(Node n l e) -> (Node n 0 0)) ns) (map (\(Edge f t c r) -> (Edge f t c c)) es)) 
{-
transferFlowFromSource :: (Num a) => Graph a -> Graph a
transferFlowFromSource (Graph so si ns es) = (Graph newSo si newNs newEs)
    where newEs = map (\(Edge f t c r)->(Edge f t c 0)) edgesFromSource
	      newNs = map (\(Node n h e)->(Node n h (capacity $ head $ (filter (\(Edge f _ _)->f == n) edgesFromSource))) nodesConnectedToSource
		  newSo = (Node ((num so) (height so) (sum capacity edgesFromSource)
          where edgesFromSource = filter (num from == num source) edges
	            nodesConnectedToSource = filter ((\(Node n h e) -> (any (\(Edge f t c r)->(num t == n) edgesFromSource)) nodes 
-}	

{-
addReverseEdges :: (Num a) => [Edge a] -> [Edge a]
addReverseEdges [] = []
addReverseEdges ((Edge f t c r):edges) = if (any (\(Edge f1 t1 _ _)->(num f1 == num t && num t1 == num f)) edges) then (Edge f t c r) : addReverseEdges edges
                                                                                                  else (Edge f t c r) : (Edge t f 0 0) : addReverseEdges edges
																								  -}
																			
relabel :: Node a -> Node a
relabel (Node n h e) = (Node n (h + 1) e)

push (Graph source sink nodes edges) (Node n1 h1 e1) (Node n2 h2 e2) edge@(Edge f t c r) = Graph source sink (((Node n1 h1 (e1 - delta))) : (Node n2 h2 (e2 + delta)) : [n | n <- nodes, num n /= n1 && num n /= n2]) ((Edge f t c (r - delta)):(filter (\e -> (num $ from $ e) /= n1 ||(num $ to $ e) /= n2) edges))
    where delta = min e1 (reserve edge)