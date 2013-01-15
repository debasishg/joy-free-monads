{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module JoyO where

import Control.Monad
import Control.Monad.Operational

data JoyInstruction a where
  Push  :: Int -> JoyInstruction ()
  Add   :: JoyInstruction ()
  Mult  :: JoyInstruction ()
  Dup   :: JoyInstruction ()
  End   :: JoyInstruction () 

type JoyProgram a = Program JoyInstruction a
type Stack a = [a]

push :: Int -> JoyProgram ()
push = singleton . Push

add :: JoyProgram ()
add = singleton Add

mult :: JoyProgram ()
mult = singleton Mult

dup :: JoyProgram ()
dup = singleton Dup

end :: JoyProgram ()
end = singleton End

interpret :: JoyProgram a -> (Stack Int -> Int)
interpret = eval . view
  where
  eval :: ProgramView JoyInstruction a -> (Stack Int -> Int)
  eval (Push a :>>= is) stack     = interpret (is ()) (a:stack)
  eval (Add    :>>= is) (a:b:stack) = interpret (is () ) (a+b:stack)
  eval (Mult   :>>= is) (a:b:stack) = interpret (is () ) (a*b:stack)
  eval (Dup    :>>= is) (a:stack) = interpret (is () ) (a:a:stack)
  eval (End    :>>= is) (result : []) = result

instance (a ~ ()) => Num (JoyProgram a) where fromInteger = push . fromInteger

-- | This joy combinator adds 1 to a number. 
incr :: JoyProgram ()
incr = do {1; add}

-- | This joy combinator increments twice
add2 :: JoyProgram ()
add2 = do {incr; incr}

-- | This joy combinator squares a number
square :: JoyProgram ()
square = do {dup; mult}

-- | This joy combinator cubes a number
cube :: JoyProgram ()
cube = do {dup; dup; mult; mult}

p :: JoyProgram ()
p = do push 5
       push 6
       add
       incr
       add2
       square
       cube
       end

