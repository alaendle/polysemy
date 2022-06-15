{-# LANGUAGE NumDecimals #-}

module AsyncSpec where

import qualified Control.Concurrent.Async as A
import Control.Concurrent.MVar
import Control.Monad
import Polysemy
import Polysemy.Async
import Polysemy.Output
import Polysemy.State
import Polysemy.Trace
import Test.Hspec

-- >>> spec
spec :: Spec
spec = describe "async" $ do
  it "should thread state and not lock" $ do
    (ts, (s, r)) <- runFinal    
                  . asyncToIOFinal
                  . embedToFinal @IO
                  . outputToIOMonoid pure
                  . traceToOutput
                  . stateToIO "hello" $ do
      let message :: Member Trace r => Int -> String -> Sem r ()
          message n msg = trace $ mconcat
            [ show n, "> ", msg ]
      ~[lock1, lock2] <- embedFinal $
        replicateM 2 newEmptyMVar
      a1 <- async @A.Async $ do
          v <- get @String
          message 1 v
          put $ reverse v

          embedFinal $ putMVar lock1 ()
          embedFinal $ takeMVar lock2
          get >>= message 1

          get @String

      void $ async @A.Async $ do
          embedFinal $ takeMVar lock1
          get >>= message 2
          put "pong"
          embedFinal $ putMVar lock2 ()

      await a1 <* put "final"

    ts `shouldContain` ["1> hello", "2> olleh", "1> pong"]
    s `shouldBe` "final"
    r `shouldBe` Just "pong"
