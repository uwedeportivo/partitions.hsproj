module Partitions 
   (
    firstPartition 
   ,nextPartition
   ,partitions
   ,firstPartitionOfSize
   ,nextPartitionOfSize
   ,partitionsOfSize
   ) 
where

-- | Drops ones from list adding them to 
-- accumulator.
dropPrefixOnes :: Int           -- ^ accumulator
               -> [Int]         -- ^ list to process
               -> (Int, [Int])  -- ^ acc and list with dropped ones

dropPrefixOnes acc [] = (acc, [])
dropPrefixOnes acc (x:xs) = if x == 1 
                            then (dropPrefixOnes (acc+1) xs) 
                            else (acc, (x:xs))
                            
-- | Modifies a list x:xs into 
-- r:(x-1):(x-1)...q times...(x-1):xs such that r < (x-1) and
-- r + (x-1) * q  = m + x
padLeft :: Int   -- ^ m
        -> [Int] -- ^ list with at least one element
        -> [Int] -- ^ modified list

padLeft m (x:xs) = padWith (m+x) (x-1) xs
  where
    padWith :: Int -> Int -> [Int] -> [Int]
    padWith 0 q xs = xs
    padWith m q xs = if (m >= q) 
                     then (padWith (m - q) q (q:xs)) 
                     else m:xs

-- | Given an integer returns the first partition
-- in reverse lexicographic order.                     
firstPartition :: Int
               -> [Int]

firstPartition n = [n]               

-- | Given a partition returns the next partition
-- in reverse lexicographic order if it exists.                     
nextPartition :: [Int]       -- ^ partition
              -> Maybe [Int] -- ^ next partition

nextPartition xs = let 
    (m, ys) = dropPrefixOnes 0 xs
    zs = padLeft m ys    
 in
    if (length ys) == 0 then Nothing else Just zs
    
-- | Returns a list of all partitions
-- of given integer.  
partitions :: Int -> [[Int]]

partitions n = partitions' [(firstPartition n)]
  where
    partitions' (p:ps) = case (nextPartition p) of 
          (Just p') -> partitions' (p':(p:ps))
          Nothing -> p:ps


-- | Returns the first partition of a certain size (number of parts).
firstPartitionOfSize :: Int   -- ^ n: integer to be partitioned
                     -> Int   -- ^ k: into this many parts
                     -> [Int] -- ^ first partition of n into k parts
                              --   in lexicographic order
                     
firstPartitionOfSize n k = (n - k + 1):(take (k-1) (repeat 1))

-- Given an integer list [a_1, a_2, ..., a_n]
-- ListInfo captures:
-- 
-- pos   = j then a_j < a_1 - 1 and a_i >= a_1 - 1 for all 2 <= i < j
-- first = a_1
-- aj    = a_j
-- acc   = sum(a_i, 1 <= i <= pos)
-- rest  = [a_{j+1}, a_{j+2}, ..., a_n]
--
data ListInfo = ListInfo
    { acc      :: Int
    , first    :: Int
    , aj       :: Int
    , pos      :: Int
    , rest     :: [Int]
    } deriving (Eq, Show)
    
listInfo :: [Int] -> Maybe ListInfo

listInfo (x:(y:xs)) = findPos ListInfo {acc=(x+y)
                                       ,first=x
                                       ,aj=(-1)
                                       ,pos=2
                                       ,rest=xs
                                       }
   where
     findPos :: ListInfo -> Maybe ListInfo
     findPos li = if (rest li) == []
             then Nothing
             else
               let (a:ass) = (rest li)
                   j = (pos li)
                   a1 = (first li)
                   ac = (acc li)
               in
                 if (a >= (a1 - 1)) 
                 then findPos (li {pos=(j+1), rest=ass, acc=(ac+a)})
                 else Just (li {pos=j, acc=(ac+a), aj=a, rest=ass})
                   
                     
-- | Prepends an element to a list n times.                        
prependN :: Int  -- ^ How many times to prepend
         -> a    -- ^ What to prepend
         -> [a]  -- ^ To which list
         -> [a]  -- ^ Resulting list
         
prependN n x xs = if n > 0 
                  then prependN (n-1) x (x:xs)
                  else xs
                      
generateNext :: ListInfo -> [Int]
generateNext li = (ac - (a+1) * j):(prependN j (a+1) ass)
    where
      j = (pos li)
      a = (aj li)
      ac = (acc li)
      ass = (rest li)

-- | Given a partition returns the next partition 
-- in lexicographic order with same number of parts
-- if it exists.                     
nextPartitionOfSize :: [Int] -> Maybe [Int]

nextPartitionOfSize (x:(y:xs)) = if (y < x - 1) 
                           then Just ((x-1):((y+1):xs))
                           else 
                             do mj <- (listInfo (x:(y:xs)))
                                return (generateNext mj)

-- | Returns a list of all partitions
-- of given integer into this many parts.
partitionsOfSize :: Int -> Int -> [[Int]]

partitionsOfSize n k = partitionsOfSize' [(firstPartitionOfSize n k)]
  where
    partitionsOfSize' (p:ps) = case (nextPartitionOfSize p) of 
            (Just p') -> partitionsOfSize' (p':(p:ps))
            Nothing -> p:ps



