module Utils 
    ( removeFirst
    , updatePair
    , inBounds
    , conMap
    , (~+), (~-), (~:), (~++)
    ) where

removeFirst :: Eq a => (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst f (x:xs) = if f x then xs else x : removeFirst f xs


updatePair :: Eq a => a -> (b -> b) -> [(a, b)] -> [(a, b)]
updatePair _ _ [] = []
updatePair k f ((k', b):xs)
  | k == k'    = (k', f b) : xs
  | otherwise  = (k', b)   : updatePair k f xs


inBounds :: Ord a => a -> (a, a) -> Bool
inBounds x (l, h) = x >= l && x <= h


conMap :: (a -> a) -> (a -> Bool) -> [a] -> [a]
conMap _ _ []     = []
conMap f c (x:xs) = (if c x then f x else x) : conMap f c xs



-- so that we can add Pos2D to eachother
(~+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(~+) (x, y) (x', y') = (x + x', y + y')

(~-) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(~-) (x, y) (x', y') = (x - x', y - y')


-- Maybe version of `:`
(~:) :: Maybe a -> [a] -> [a]
(~:)  Nothing xs = xs
(~:) (Just x) xs = x : xs

-- Maybe version of `++`
(~++) :: Maybe [a] -> [a] -> [a]
(~++)  Nothing xs = xs
(~++) (Just x) xs = x ++ xs
