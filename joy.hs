{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveFunctor, FlexibleContexts, UndecidableInstances, TypeFamilies #-}
module Joy where

import Data.Functor ((<$>))
import Control.Monad.Free 
import Control.Monad.Free.Church 

data JoyOperator cont = Push Int cont 
                      | Add      cont 
                      | Mult     cont 
                      | Dup      cont 
                      | End          
                      deriving (Show, Functor)

-- | The free monad over JoyOperator
type Joy = Free JoyOperator

-- | Push an integer to the stack
push :: MonadFree JoyOperator m => Int -> m ()
push n = wrap $ Push n $ return ()

-- | Add the top two numbers of the stack and push the sum
add :: MonadFree JoyOperator m => m ()
add = wrap $ Add $ return ()

-- | Multiply the top two numbers of the stack and push the product
mult :: MonadFree JoyOperator m => m ()
mult = wrap $ Mult $ return ()

-- | Duplicate the number from the top of the stack and push it to the top
dup :: MonadFree JoyOperator m => m ()
dup = wrap $ Dup $ return ()

-- | End of program: Ignore everything after it
end :: MonadFree JoyOperator m => m ()
end = wrap $ End

-- | Pretty print my Joy program: an interpreter over Free monad
printProgram :: Show n => Joy n -> String
printProgram (Free (Push v c))     = "push " ++ show v ++ " " ++ printProgram c
printProgram (Free (Add c))        = "add " ++ printProgram c
printProgram (Free (Mult c))       = "mult " ++ printProgram c
printProgram (Free (Dup c))        = "dup " ++ printProgram c
printProgram (Free End)            = ""
printProgram Pure{}                = error "Program not ended!"

-- | These are the different errors we can get when running a program.
data JoyError = NotEnoughParamsOnStack
                | NotEmptyOnEnd
                | NoEnd
                deriving (Show, Eq)

-- | Run a joy program. Result is either an Int or an error
runProgram :: Joy n -> Either JoyError Int
runProgram program = joy [] program
  where joy stack (Free (Push v cont)) = joy (v:stack) cont
        joy (a:b:s) (Free (Add cont))  = joy (a + b : s) cont
        joy (a:b:s) (Free (Mult cont)) = joy (a * b : s) cont
        joy (a:s) (Free (Dup cont))    = joy (a : a : s) cont
        joy _ (Free Add {})            = Left NotEnoughParamsOnStack
        joy _ (Free Dup {})            = Left NotEnoughParamsOnStack
        joy [] (Free End)              = Left NotEnoughParamsOnStack
        joy [result] (Free End)        = Right result
        joy _ (Free End)               = Left NotEmptyOnEnd
        joy _ Pure {}                  = Left NoEnd
        
instance (MonadFree JoyOperator m, a ~ ()) => Num (m a) where fromInteger = push . fromInteger

-- | This joy combinator adds 1 to a number. 
incr :: MonadFree JoyOperator m => m ()
incr = do {1; add}

-- | This joy combinator increments twice
add2 :: MonadFree JoyOperator m => m ()
add2 = do {incr; incr}

-- | This joy combinator squares a number
square :: MonadFree JoyOperator m => m ()
square = do {dup; mult}

-- | This joy combinator cubes a number
cube :: MonadFree JoyOperator m => m ()
cube = do {dup; dup; mult; mult}

p :: Joy ()
p = do improve $ push 5
       improve $ push 6
       improve $ add
       improve $ incr
       improve $ add2
       improve $ square
       improve $ cube
       improve $ end

