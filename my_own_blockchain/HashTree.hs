-- author: Monika Michaluk
module HashTree where
import Hashable32 ( Hashable(hash), Hash, showsHash )
import Utils
import PPrint

data Tree a = Node Hash (Tree a) (Tree a) | Leaf Hash a | Empty

type MerklePath = [Either Hash Hash]

data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
    showsPrec p (MerkleProof x path) = showParen (p >= 6) (showString "MerkleProof " . showsPrec 11 x . showChar ' ' . showsMerklePath path)


treeHash :: Tree a -> Hash
treeHash t = case t of
    Node h _ _ -> h
    Leaf h x -> h
    Empty -> error "You can't get hash from an empty tree!"

leaf :: Hashable a => a -> Tree a
leaf x = Leaf (hash x) x

twig :: Hashable a => Tree a -> Tree a
twig t = Node (hash (h,h)) t Empty where
    h = treeHash t

node :: Hashable a =>  Tree a -> Tree a -> Tree a
node t1 t2 = Node (hash (h1,h2)) t1 t2 where
    h1 = treeHash t1
    h2 = treeHash t2

drawTree :: Show a => Tree a -> String
drawTree t = drawTreeHelper t 0 "" where
    drawTreeHelper :: Show a => Tree a -> Int -> ShowS
    drawTreeHelper t d = case t of
        Empty -> id
        Leaf h x -> showSpaces d . showsHash h . showChar ' ' . shows x . showNewLine
        Node h t1 t2 -> showSpaces d . showsHash h . showChildNr (1 + isNotEmpty t2) . drawTreeHelper t1 d' . drawTreeHelper t2 d' where
            d' = d + 1
            isNotEmpty Empty = 0
            isNotEmpty _ = 1
    showSpaces d = showString $ replicate (2*d) ' '
    showChildNr n = showChar ' ' . (if n == 1 then showChar '+' else showChar '-') . showNewLine


-- Tworzy kolejne 'poziomy' drzewa - dostaje listę wierzchołków-dzieci i łącząc je w pary tworzy listę wierzchołków będących
-- rodzicami tych wierzchołków. Powtarza to dopóki lista nie będzie jednoelementowa - jest to korzeń drzewa.
buildTreeHelper :: Hashable a => [Tree a] -> Tree a
buildTreeHelper [x] = x -- korzeń
buildTreeHelper l = buildTreeHelper $ buildLevel l where
    buildLevel l = case l of
        [] -> error "Empty list!"
        [x] -> [twig x]
        [x, y] -> [node x y]
        (x:y:t) -> node x y : buildLevel t -- ogon t jest na pewno niepusty

buildTree :: Hashable a => [a] -> Tree a
buildTree l = buildTreeHelper $ map leaf l


showsMerklePath :: MerklePath -> ShowS
showsMerklePath [] = id
showsMerklePath (h:t) = showChar dir . showsHash hash . showsMerklePath t where
    hash = fromEither h
    dir = case h of
        Right _ -> '>' 
        Left _ -> '<'

showMerklePath :: MerklePath -> String
showMerklePath path = showsMerklePath path ""


merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x t = merklePathsHelper t [] where
    merklePathsHelper Empty _ = []
    merklePathsHelper (Leaf h _) p = if h == hash x then [reverse p] else []
    merklePathsHelper (Node h t1 Empty) p = merklePathsHelper t1 (Left h1 : p) where
        h1 = treeHash t1
    merklePathsHelper (Node h t1 t2) p = merklePathsHelper t1 (Left h2 : p) ++ merklePathsHelper t2 (Right h1 : p) where
        h1 = treeHash t1
        h2 = treeHash t2


buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof x t = MerkleProof x <$> maybeHead (merklePaths x t)


verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof x path) = h == calcPathRootHash path where
    calcPathRootHash [] = hash x
    calcPathRootHash (head:t) =
        let other = calcPathRootHash t in
            case head of
                Right h -> hash (h, other)
                Left h -> hash (other, h)
