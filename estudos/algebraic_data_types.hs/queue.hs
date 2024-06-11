module Queue(
    Queue,
    emptyQ,   -- Queue a 
    isEmptyQ, -- Queue a -> Bool
    addQ,     -- a -> Queue a -> Queue a
    remQ      -- Queue a -> (a, Queue a)
) where

newtype Queue a =  Queue [a]

emptyQ :: Queue a 
emptyQ = Queue []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue []) = True
isEmptyQ _ = False

addQ :: a -> Queue a -> Queue a 
addQ x (Queue xs) = Queue (xs++[x])

remQ :: Queue a -> (a, Queue a)
remQ q@(Queue xs) 
    | not (isEmptyQ q) = (head xs, Queue(tail xs))
    | otherwise = error "remQ"