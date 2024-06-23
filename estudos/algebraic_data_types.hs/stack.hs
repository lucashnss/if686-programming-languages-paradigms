module Stack(
    Stack,
    push, -- Stack a -> a -> Stack a
    pop,     -- Stack a -> Stack a 
    top      -- Stack a -> a 
) where

data Stack t = Stack [t] | StackVazia
            deriving(Show)

push :: t -> Stack t -> Stack t
push x StackVazia = Stack [x]
push x (Stack t) = Stack (x:t)

pop :: Stack t -> Stack t 
pop StackVazia  = error "Stack Vazia"
pop (Stack (x:xs)) = Stack xs  

top :: Stack t -> t 
top StackVazia = error "Stack Vazia"
top (Stack (x:xs)) = x 