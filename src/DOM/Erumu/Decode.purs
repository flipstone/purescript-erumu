module DOM.Erumu.Decode
  ( Decode
  , run
  , runOrCrash
  , event
  , domValue
  , domEffect
  , crash
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Web.Event.Event (Event)

newtype Decode a = Decode (Event -> Effect (Either String a))

run :: forall a. Decode a -> Event -> Effect (Either String a)
run (Decode r) = r

runOrCrash :: forall a. Decode a -> Event -> Effect a
runOrCrash decode evt = do
  result <- run decode evt

  case result of
    Right a -> pure a
    Left err -> unsafeCrashWith ("DOM.Erumu.Decode.runOrCrash: " <> err)

event :: Decode Event
event = Decode $ \e -> pure (pure e)

domValue :: forall a b. (a -> Effect b) -> a -> Decode b
domValue f a = Decode $ \_ -> pure <$> f a

domEffect :: Effect Unit -> Decode Unit
domEffect eff = Decode $ \_ -> pure <$> eff

crash :: forall a. String -> Decode a
crash = Decode <<< const <<< pure <<< Left

instance functorDecode :: Functor Decode where
  map f (Decode dA) = Decode (map (map (map f)) dA)

instance applyDecode :: Apply Decode where
  apply (Decode dF) (Decode dA) = Decode $ \e -> apply (map apply (dF e)) (dA e)

instance applicativeDecode :: Applicative Decode where
  pure = Decode <<< pure <<< pure <<< pure

instance bindDecode :: Bind Decode where
  bind (Decode dA) f = Decode $ \e -> do
    result <- dA e
    case f <$> result of
      Right (Decode dB) -> dB e
      Left err -> pure (Left err)

instance monadDecode :: Monad Decode

