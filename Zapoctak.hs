{-
**********************
***** Data types *****
**********************
-}

data Node a = Node { num :: Int, height :: Int, excess :: a } deriving (Eq, Show)

data Edge a = Edge { from :: Int, to :: Int, capacity :: a, reserve :: a } deriving (Eq, Show)

data Graph a = Graph { src :: Node a, snk :: Node a, nodes :: [Node a], edges :: [Edge a]}

{-
************************
***** Testing data *****
************************
-}

--1. Graph
node1 = Node 1 0 0
node2 = Node 2 0 0 
node3 = Node 3 0 0 
sink = Node 4 0 0
source = Node 0 0 0

gr = Graph source sink [source, sink, node1,node2,node3] [Edge 0 1 5 5, Edge 0 2 4 4, Edge 2 1 6 6, Edge 1 3 7 7, Edge 2 4 3 3, Edge 3 4 3 3]

--2. Graph
source2 = Node 0 0 0
sink2 = Node 10 0 0
node11 = Node 1 0 0
node12 = Node 2 0 0
node13 = Node 3 0 0
node14 = Node 4 0 0
node15 = Node 5 0 0
node16 = Node 6 0 0
node17 = Node 7 0 0
node18 = Node 8 0 0
node19 = Node 9 0 0

edges2 = [Edge 0 1 4.2 4.2, Edge 0 2 5 5, Edge 0 3 17.77 17.77, Edge 3 0 15.25 15.25, Edge 3 4 2 2, Edge 3 5 7 7, Edge 5 3 8.25 8.25, Edge 2 6 9 9, Edge 2 7 11 11, Edge 7 8 3 3, Edge 3 8 5 5, Edge 8 2 11 11, Edge 1 10 5 5, Edge 4 9 5 5, Edge 8 10 12 12, Edge 9 10 7 7, Edge 5 8 7.75 7.75, Edge 6 7 4.44 4.44, Edge 7 5 7.55 7.55, Edge 9 1 6 6, Edge 8 9 10 10]

gr2 = Graph source2 sink2 [source2,sink2,node11,node12,node13,node14,node15,node16,node17,node18,node19] edges2



t1e = edges $ goldberg $ gr
t1n = nodes $ goldberg $ gr

t2e = edges $ goldberg $ gr2
t2n = nodes $ goldberg $ gr2



{-
*******************
***** Program *****
*******************
-}


initialize :: (Num a, Ord a) => Graph a -> Graph a
initialize gr = transferFlowFromSource $ initializePreFlow $ gr

goldberg :: (Ord a, Num a) => Graph a -> Graph a
goldberg gr = pushRelabel $ last $ takeWhile (\gr -> not $ null $ (nodesWithExcess gr)) (iterate pushRelabel (initialize gr))

initializePreFlow :: (Num a) => Graph a -> Graph a
initializePreFlow gr@(Graph so si ns es) = Graph so si (newSource : otherNodes) (addReverseEdges es es)
    where otherNodes = [ n | n <- ns, num n /= num so]
          newSource = (Node (num so) (length ns) infinity)
              where infinity = sum (map capacity es)

transferFlowFromSource :: (Num a, Ord a) => Graph a -> Graph a
transferFlowFromSource gr = foldr (\e g -> push g e) gr (edgesFromSource gr)

pushRelabel :: (Num a, Ord a) => Graph a -> Graph a
pushRelabel gr@(Graph _ _ ns es) 
    | null $ nodesWithExcess $ gr = gr
    | chosenEdge /= Nothing = push gr (unJust chosenEdge) 
    | otherwise = relabel gr (head $ nodesWithExcess $ gr)
    where chosenEdge = safeHead $ filter (\e -> (reserve e > 0) && (from e) `elem` (map num (nwe)) && (hgt (from e) > hgt (to e))) (es)
          nwe = nodesWithExcess gr
          hgt x = height (unJust (numToNode ns x))


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


edgesFromSource :: Graph a -> [Edge a]
edgesFromSource (Graph sr sn ns es) = filter (\e -> (from e) == (num sr)) es

edgesFromNode :: Graph a -> Node a -> [Edge a]
edgesFromNode (Graph _ _ ns es) (Node n _ _) = filter (\e -> (from $ e) == n) es


addReverseEdges :: (Num a) => [Edge a] -> [Edge a] -> [Edge a]
addReverseEdges [] _ = []
addReverseEdges (e@(Edge f t c r):edges) allEdges = if (hasReverseEdge) then e : addReverseEdges edges allEdges
                                                                        else e : (Edge t f c 0) : addReverseEdges edges allEdges
    where hasReverseEdge = any (\(Edge f1 t1 _ _)->(f1 == t && t1 == f)) allEdges


nodesWithExcess ::(Ord a, Num a) => Graph a -> [Node a]
nodesWithExcess (Graph sr sn ns _) = filter (\(Node n h e) -> e>0 && n /= num sr && n /= num sn) ns


relabel :: Graph a -> Node a -> Graph a
relabel (Graph sr sn ns es) (Node n h e) = Graph sr sn ((Node n (h + 1) e):otherNodes) es
    where otherNodes = [x | x<-ns, (num x) /= n] 


push :: (Num a, Ord a) => Graph a -> Edge a -> Graph a
push (Graph source sink ns es) edge@(Edge f t c r) = Graph source sink (newNd1 : newNd2 : otherNodes) (newEd : newRevEd : otherEdges)
    where otherEdges = filter (\e -> ((from e) /= f ||(to e) /= t) && ((from e) /= t || (to e) /= f)) es
          delta = min (excess nd1) (reserve edge)
          nd1 = unJust (numToNode ns f)
          nd2 = unJust (numToNode ns t)
          newNd1 = Node f (height nd1) ((excess nd1) - delta)
          newNd2 = Node t (height nd2) ((excess nd2) + delta)
          otherNodes = [n | n <- ns, num n /= t && num n /= f]
          revEd = unJust (find (\e -> from e == t && to e == f) es)
          newEd = Edge f t c (r - delta)
          newRevEd = (Edge t f (capacity revEd) ((reserve revEd) + delta))


{-
****************************************
***** Helper functions for testing *****
****************************************
-}
edgeInfo :: Edge a -> (Int, Int, a, a)
edgeInfo (Edge t f c r) = (t,f,c,r)

nodeInfo :: Node a -> (Int, Int, a)
nodeInfo (Node n h e) = (n, h, e)

edgeList :: Graph a -> [(Int, Int, a, a)]
edgeList gr = map edgeInfo (edges gr)

nodeList :: Graph a -> [(Int, Int, a)]
nodeList gr = map nodeInfo (nodes gr)

nthIteration :: (Num a, Ord a) => Graph a -> Int -> Graph a
nthIteration gr n = (iterate pushRelabel (initialize gr)) !! n


