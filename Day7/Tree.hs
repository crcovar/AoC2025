module Tree
  ( Tree,
    singleton,
    insert,
    insertWith,
    remove,
    size,
    find,
  )
where

data Tree a = EmptyTree | Node (Int, a) (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node (1, x) EmptyTree EmptyTree

insert :: (Ord a) => a -> Tree a -> Tree a
insert = insertWith 1

insertWith :: (Ord a) => Int -> a -> Tree a -> Tree a
insertWith n x EmptyTree = insertWith (n - 1) x $ singleton x
insertWith n x (Node (c, a) left right)
  | x == a = Node (c + n, x) left right
  | x < a = Node (c, a) (insertWith n x left) right
  | otherwise = Node (c, a) left (insertWith n x right)

remove :: (Ord a) => a -> Tree a -> Tree a
remove _ EmptyTree = EmptyTree
remove x (Node (c, a) l r)
  | x == a = Node (0, a) l r
  | x < a = Node (c, a) (remove x l) r
  | x > a = Node (c, a) l (remove x r)

size :: Tree a -> Int
size EmptyTree = 0
size (Node (c, _) l r) = c + size l + size r

findBy :: (Ord a) => (a -> a -> Ordering) -> a -> Tree a -> Maybe (Int, a)
findBy _ _ EmptyTree = Nothing
findBy f x (Node (c, a) l r) = case f x a of
  EQ -> Just (c, a)
  LT -> findBy f x l
  GT -> findBy f x r

find :: (Ord a) => a -> Tree a -> Maybe (Int, a)
find = findBy compare

memberBy :: (Ord a) => (a -> a -> Ordering) -> a -> Tree a -> Bool
memberBy f x t = case findBy f x t of
  Nothing -> False
  Just _ -> True

member :: (Ord a) => a -> Tree a -> Bool
member = memberBy compare
