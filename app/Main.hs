{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
import           Control.Monad
import           Control.Monad.Random
import           Data.List ( foldl' )

import qualified Numeric.LinearAlgebra.Static as SA

import           Test.QuickCheck
import           Grenade

main::IO ()
main = test 100000

type MyNet = Network '[ FullyConnected 2 40, Tanh, FullyConnected 40 10, Relu, FullyConnected 10 1, Logit ]
                     '[ 'D1 2, 'D1 40, 'D1 40, 'D1 10, 'D1 10, 'D1 1, 'D1 1]

-- this network not worked for me
type MyNet_xor = Network '[ FullyConnected 2 2, FullyConnected 2 1 ]
                         '[ 'D1 2,'D1 2, 'D1 1]

randNet :: MonadRandom m => m MyNet
randNet = randomNetwork

r::LearningParameters
r = LearningParameters 0.01 0.95 0.0005

test::Int->IO ()
test n = do 
 net0 <- randNet
 trainedNet <- netTrain net0 r n
 verboseCheck (prop_booleanXor trainedNet)

netTrain::MyNet->LearningParameters -> Int-> IO MyNet
netTrain net0 rate numIterations= do 
    let inputs =  S1D . SA.vector <$>[[0,0],[0,1],[1,0],[1,1]]
        outputs = S1D . SA.vector <$>[[0],[1],[1],[0]]
    foldM (runIteration inputs outputs) net0 [1..numIterations]
    where
     runIteration inps outs net _ = pure $ foldl' trainEach net (zip inps outs)
     trainEach !network (i,o) = train rate network i o

---------------------useful for quickCheck --------------------------------

toBool:: S ('D1 1) -> Bool
toBool (S1D row) = (SA.mean row) > 0.5

refxor::Bool->Bool ->Bool
refxor True = not
refxor False = id

prop_booleanXor::MyNet->(Bool,Bool)->Bool
prop_booleanXor net (x,y) =
 let ans =toBool.runNet  net .S1D . SA.vector $(fromIntegral.boolToInt)<$> [x,y]
 in ans == refxor x y

boolToInt::Bool->Int
boolToInt True = 1
boolToInt _ = 0
