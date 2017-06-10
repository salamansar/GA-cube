{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module CongruentGenerator where

import Utils
import Control.Lens
import Control.Monad.State

data RandomContext = RandomContext {_a :: Int, _c :: Int, _m :: Int, _token :: Int}
   deriving Show
   
makeLenses ''RandomContext

simpleRndContext :: Int -> RandomContext
simpleRndContext t = RandomContext {_a = 106, _c = 1283, _m = 6075, _token = t}

rand :: Int -> RandomContext -> (Int, RandomContext)
rand 0 context = (0, context)
rand upBound context = 
   let nextContext = nextRnd context
   in ((mod (nextContext^.token) upBound), nextContext)


nextRnd :: RandomContext -> RandomContext
nextRnd ctx@RandomContext{..} = 
   let nextVal = mod (_a * _token + _c) $ _m
   in ctx & token .~ nextVal

randVector :: Int -> Int -> RandomContext -> ([Int], RandomContext)
randVector 0 upBound context = ([], context)
randVector count upBound context = 
   let (vals, gens) = unzip $ take count $ randVector3 upBound context
   in (vals, head $ reverse gens) 
   
randVector3 :: Int -> RandomContext -> [(Int, RandomContext)]
randVector3 upBound context = iterate nextVal $ nextVal (0, context)
   where nextVal = \(_, ctx) -> rand upBound ctx
   
-- generator for State monad
randVectorS :: Int -> Int -> State RandomContext [Int]
randVectorS count upBound =
   do ctx <- get
      let (v, newCtx) = randVector count upBound ctx
      put newCtx
      return v

