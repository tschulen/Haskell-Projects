-- Tyler Schulenberg
-- CMPS 112
-- Haskell HW # 3

data BST k v = Empty |
               Node k v (BST k v) (BST k v)
-- 1
-- Suppose we have the following type of binary search trees with keys of type k and values of type v:
-- data BST k v = Empty |
--                Node k v (BST k v) (BST k v)
-- Write a val function that returns Just the stored value at the root node of the tree. If the tree is empty, val returns Nothing.
-- val :: BST k v -> Maybe v
			  
val :: BST k v -> Maybe v
val(Empty) = Nothing
val(Node k v _ _) = Just v

-- 2
-- Write a size function that returns the number of nodes in the tree.
-- size :: BST k v -> Int

size :: BST k v -> Int
size(Empty) = 0
size(Node k v x y) = 1 + size x + size y

-- 3
-- Write an ins function that inserts a value v using k as key. If the key is already used in the tree, it just updates the value, otherwise it will add a new node while maintaining the order of the binary search tree.
-- ins :: (Ord k) => k -> v -> BST k v -> BST k v

ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins k v Empty = Node k v Empty Empty
ins k v (Node a b c d)
	| k == a = Node k v c d
	| k < a = Node a b (ins k v c) d
	| k > a = Node a b c (ins k v d)
	
-- 4
-- Make BST an instance of the Show type class. You will have to implement the show function such that the result every node of the tree is shown as "(leftTree value rightTree)".
instance (Show v) => Show (BST k v) where
	show Empty = ""
	show (Node k v left right) = "(" ++ show left ++ show v ++ show right ++ ")"

-- 5
-- Make JSON an instance of the Show type class. You will have to implement the show function such that the output looks like normal JSON.
-- Suppose we have the following type

data JSON = JStr String
          | JNum Double
          | JArr [JSON]
          | JObj [(String, JSON)]
		  
instance Show JSON where
	show (JStr s) = show s
	show (JNum d) = show d
	show (JArr a) = show a
	show (JObj b) = "{" ++ helpMe b ++ "}"

helpMe :: [(String, JSON)] -> String
helpMe [] = ""
helpMe ((x,y):tail) = show x ++ ":" ++ show y ++ "," ++ helpMe tail

-- 6
-- Make Double and lists of Json-things members of the type class Json. You will have to implement toJson and fromJson for each of these types.

class Json a where
	toJson :: a -> JSON
	fromJson :: JSON -> a
	
instance Json Double where 
	toJson x = JNum x
	fromJson (JNum x) = x
	
instance (Json a) => Json [a] where
	toJson x = JArr (map toJson x)
	fromJson (JArr xs) = map (fromJson) xs
