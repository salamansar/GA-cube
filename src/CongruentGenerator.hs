{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module CongruentGenerator where

import Utils

data RandomContext = RandomContext {a :: Int, c :: Int, m :: Int, token :: Int}
   deriving Show

simpleRndContext :: Int -> RandomContext
simpleRndContext t = RandomContext {a = 106, c = 1283, m = 6075, token = t}

rand :: Int -> RandomContext -> (Int, RandomContext)
rand 0 context = (0, context)
rand upBound context = 
   let nextContext@RandomContext {token = t} = nextRnd context
   in ((mod t upBound), nextContext)


nextRnd :: RandomContext -> RandomContext
nextRnd RandomContext {..} = 
   let nextVal = mod (a * token + c) m
   in RandomContext a c m nextVal

randVector :: Int -> Int -> RandomContext -> ([Int], RandomContext)
randVector 0 upBound context = ([], context)
randVector count upBound context = 
   let (vals, gens) = unzip $ take count $ randVector3 upBound context
   in (vals, head $ reverse gens) 
   
randVector3 :: Int -> RandomContext -> [(Int, RandomContext)]
randVector3 upBound context = iterate nextVal $ nextVal (0, context)
   where nextVal = \(_, ctx) -> rand upBound ctx
   


