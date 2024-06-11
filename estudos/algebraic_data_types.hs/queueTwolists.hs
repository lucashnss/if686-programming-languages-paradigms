--  Queue with addQ and remQ substancially more efficient than Queues with single list
module QueueTwoLists(
    Queue,
    emptyQ,   -- Queue a 
    isEmptyQ, -- Queue a -> Bool
    addQ,     -- a -> Queue a -> Queue a
    remQ      -- Queue a -> (a, Queue a)
) where

data Queue a = Queue [a] [a]

emptyQ :: Queue a 
emptyQ = Queue [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue [] []) = True
isEmptyQ _  = False

addQ :: a -> Queue a -> Queue a 
addQ x (Queue xs ys) = Queue xs (x:ys)

remQ :: Queue a -> (a,Queue a)
remQ (Queue (x:xs) ys) = (x, Queue xs ys)
remQ (Queue [] ys@(z:zs)) = remQ (Queue (reverse ys) [])
remQ (Queue [] []) = error "remQ"