data Node a = Node { num :: Int, height :: Int, excess :: a } deriving (Eq)

data Edge a = Edge { from :: Int, to :: Int, capacity :: a, reserve :: a } deriving (Eq)

data Graph a = Graph { src :: Node a, snk :: Node a, nodes :: [Node a], edges :: [Edge a]}

node1 = Node 1 0 0
node2 = Node 2 1 5 
node3 = Node 3 0 0 
sink = Node 4 0 0
source = Node 0 0 0

gr = Graph source sink [source, sink, node1,node2,node3] [Edge 0 1 5 5, Edge 0 2 4 4, Edge 2 1 6 6, Edge 1 3 7 7, Edge 2 4 3 3, Edge 3 4 7 7]

initialize :: (Num a, Ord a) => Graph a -> Graph a
initialize gr = transferFlowFromSource (initializePreFlow gr)

initializePreFlow :: (Num a) => Graph a -> Graph a
initializePreFlow gr@(Graph so si ns es) = (Graph so si (map (\(Node n l e) -> (Node n 0 0)) ns) (map (\(Edge f t c r) -> (Edge f t c c)) es))

pushRelabel :: (Num a, Ord a) => Graph a -> Graph a
pushRelabel gr 
    | null $ nodesWithExcess $ gr = gr
	| chosenEdge /= Nothing = push gr (unJust chosenEdge) 
	| otherwise = relabel gr (head $ nodesWithExcess $ gr)
    where chosenEdge = safeHead $ filter (\e -> (reserve e > 0) && (from e) `elem` (map num (nodesWithExcess gr)) && ((height (unJust (numToNode (nodes gr) (from e)))) > (height (unJust (numToNode (nodes gr)(to e)))))) (edges gr)

numToNode :: (Num a, Ord a) => [Node a] -> Int -> Maybe (Node a)	
numToNode ns x  = find (\n -> num n == x) ns

unJust :: Maybe a -> a
unJust (Just x) = x

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) | p x = Just x
            | otherwise = find p xs


safeHead :: (Eq a) => [a] -> Maybe a
safeHead x 
    | null x = Nothing
    | otherwise = Just $ head $ x       

	
transferFlowFromSource :: (Num a, Ord a) => Graph a -> Graph a
transferFlowFromSource gr = foldr (\e g -> push g e) gr (edgesFromSource gr)
     
{-
nodesConnectedToSource :: (Num a, Ord a) => Graph a -> [Int]
nodesConnectedToSource gr = map (\e -> unJust $ numToNode (nodes gr) (to e)) (edgesFromSource gr)

nodesConnectedToNode :: Graph a -> Node a -> [Node a]
nodesConnectedToNode gr n = map to (edgesFromNode gr n)
-}

edgesFromSource :: Graph a -> [Edge a]
edgesFromSource (Graph sr sn ns es) = filter (\e -> (num $ from $ e) == (num sr)) es

edgesFromNode :: Graph a -> Node a -> [Edge a]
edgesFromNode (Graph _ _ ns es) (Node n _ _) = filter (\e -> (num $ from $ e) == n) es

{-
addReverseEdges :: (Num a) => [Edge a] -> [Edge a]
addReverseEdges [] = []
addReverseEdges ((Edge f t c r):edges) = if (any (\(Edge f1 t1 _ _)->(num f1 == num t && num t1 == num f)) edges) then (Edge f t c r) : addReverseEdges edges
                                                                                                  else (Edge f t c r) : (Edge t f 0 0) : addReverseEdges edges
-}

nodesWithExcess ::(Ord a, Num a) => Graph a -> [Node a]
nodesWithExcess (Graph sr sn ns _) = filter (\(Node n h e) -> e>0 && n /= num sr && n /= num sn) ns
	
	
relabel :: Graph a -> Node a -> Graph a
relabel (Graph sr sn ns es) (Node n h e) = Graph sr sn ((Node n (h + 1) e):otherNodes) es
    where otherNodes = [x | x<-ns, (num x) /= n] 
{-
push :: (Num a, Ord a) => Graph a -> Edge a -> Graph a
push (Graph source sink nodes edges) edge@(Edge f t c r) = Graph source sink (((Node (num f) (height f) ((excess f) - delta))) : (Node (num t) (height t)  ((excess t) + delta)) : [n | n <- nodes, num n /= (num f) && num n /= (num t)]) ((Edge f t c (r - delta)):(filter (\e -> (num $ from $ e) /= (num f) ||(num $ to $ e) /= (num t)) edges))
    where delta = min (excess f) (reserve edge)
	-}
	
push :: (Num a, Ord a) => Graph a -> Edge a -> Graph a
push (Graph source sink nodes edges) edge@(Edge f t c r) = Graph source sink (((Node n1 h1 (e1 - delta))) : (Node n2 h2  (e2 + delta)) : [n | n <- nodes, num n /= n1 && num n /= n2]) ((Edge f t c (r - delta)):(filter (\e -> (num $ from $ e) /= n1 ||(num $ to $ e) /= n2) edges))
    where (delta, n1, h1, e1, n2, h2, e2) = ((min (excess f) (reserve edge)), num f, height f, excess f, num t, height t, excess t)
	
	
--For testing
edgeInfo :: Edge a -> (Int, Int, a, a)
edgeInfo (Edge t f e r) = (num t,num f,e,r)
