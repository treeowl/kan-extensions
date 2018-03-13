{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if (__GLASGOW_HASKELL__ >= 708) && (__GLASGOW_HASKELL__ < 802)
{-# LANGUAGE DeriveDataTypeable #-}
#endif

-- We don't use TypeInType for 8.0 because that feature was pretty
-- unreliable then; we don't want to have to worry about GHC panics
-- behind every bush.
#if __GLASGOW_HASKELL__ >= 802
#define USE_TYPE_IN_TYPE 1
#endif

#ifdef USE_TYPE_IN_TYPE
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Codensity
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Monad.Codensity
  ( Codensity(..)
  , lowerCodensity
  , codensityToAdjunction, adjunctionToCodensity
  , codensityToRan, ranToCodensity
  , codensityToComposedRep, composedRepToCodensity
  , wrapCodensity
  , improve
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Data.Functor.Adjunction
import Data.Functor.Apply
import Data.Functor.Kan.Ran
import Data.Functor.Plus
import Data.Functor.Rep
#if (__GLASGOW_HASKELL__ >= 708) && (__GLASGOW_HASKELL__ < 800)
import Data.Typeable
#endif
#ifdef USE_TYPE_IN_TYPE
import GHC.Exts (TYPE)
import Data.Type.Equality (type (~~))
#endif

-- |
-- @'Codensity' f@ is the Monad generated by taking the right Kan extension
-- of any 'Functor' @f@ along itself (@Ran f f@).
--
-- This can often be more \"efficient\" to construct than @f@ itself using
-- repeated applications of @(>>=)@.
--
-- See \"Asymptotic Improvement of Computations over Free Monads\" by Janis
-- Voigtländer for more information about this type.
--
-- <https://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf>
#ifdef USE_TYPE_IN_TYPE
newtype Codensity (m :: k -> TYPE rep) a = Codensity
-- Note: we *could* generalize @a@ to @TYPE repa@, but the 'Functor'
-- instance wouldn't carry that, so it doesn't really seem worth
-- the complication.
#else
newtype Codensity m a = Codensity
#endif
  { runCodensity :: forall b. (a -> m b) -> m b
  }
#if (__GLASGOW_HASKELL__ >= 708) && (__GLASGOW_HASKELL__ < 800)
    deriving Typeable
#endif

#ifdef USE_TYPE_IN_TYPE
instance Functor (Codensity (k :: j -> TYPE rep)) where
#else
instance Functor (Codensity k) where
#endif
  fmap f (Codensity m) = Codensity (\k -> m (\x -> k (f x)))
  {-# INLINE fmap #-}

#ifdef USE_TYPE_IN_TYPE
instance Apply (Codensity (f :: k -> TYPE rep)) where
#else
instance Apply (Codensity f) where
#endif
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}

#ifdef USE_TYPE_IN_TYPE
instance Applicative (Codensity (f :: k -> TYPE rep)) where
#else
instance Applicative (Codensity f) where
#endif
  pure x = Codensity (\k -> k x)
  {-# INLINE pure #-}
  Codensity f <*> Codensity g = Codensity (\bfr -> f (\ab -> g (\x -> bfr (ab x))))
  {-# INLINE (<*>) #-}

#ifdef USE_TYPE_IN_TYPE
instance Monad (Codensity (f :: k -> TYPE rep)) where
#else
instance Monad (Codensity f) where
#endif
  return = pure
  {-# INLINE return #-}
  m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))
  {-# INLINE (>>=) #-}

-- Writing instances like
-- instance MonadFail f => MonadFail (Codensity f)
-- leads to some hidden flexible instances. Haddock will show things like
--
-- MonadFail f => MonadFail (Codensity * LiftedRep f)
-- 
-- Since FlexibleInstances are bad for inference, we avoid them when
-- we can by carefully pushing kind constraints to the left.

#ifdef USE_TYPE_IN_TYPE
instance (f ~~ f', Fail.MonadFail f')
  => Fail.MonadFail (Codensity (f :: k -> TYPE rep)) where
#else
instance Fail.MonadFail f => Fail.MonadFail (Codensity f) where
#endif
  fail msg = Codensity $ \ _ -> Fail.fail msg
  {-# INLINE fail #-}

#ifdef USE_TYPE_IN_TYPE
instance (m ~~ m', MonadIO m')
  => MonadIO (Codensity (m :: k -> TYPE rep)) where
#else
instance MonadIO m => MonadIO (Codensity m) where
#endif
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans Codensity where
  lift m = Codensity (m >>=)
  {-# INLINE lift #-}

#ifdef USE_TYPE_IN_TYPE
instance (v ~~ v', Alt v')
  => Alt (Codensity (v :: k -> TYPE rep)) where
#else
instance Alt v => Alt (Codensity v) where
#endif
  Codensity m <!> Codensity n = Codensity (\k -> m k <!> n k)
  {-# INLINE (<!>) #-}

#ifdef USE_TYPE_IN_TYPE
instance (v ~~ v', Plus v') => Plus (Codensity (v :: k -> TYPE rep)) where
#else
instance Plus v => Plus (Codensity v) where
#endif
  zero = Codensity (const zero)
  {-# INLINE zero #-}

{-
instance Plus v => Alternative (Codensity v) where
  empty = zero
  (<|>) = (<!>)

instance Plus v => MonadPlus (Codensity v) where
  mzero = zero
  mplus = (<!>)
-}

#ifdef USE_TYPE_IN_TYPE
instance (v ~~ v', Alternative v')
  => Alternative (Codensity (v :: k -> TYPE rep)) where
#else
instance Alternative v => Alternative (Codensity v) where
#endif
  empty = Codensity (\_ -> empty)
  {-# INLINE empty #-}
  Codensity m <|> Codensity n = Codensity (\k -> m k <|> n k)
  {-# INLINE (<|>) #-}

#ifdef USE_TYPE_IN_TYPE
instance (v ~~ v', Alternative v')
   => MonadPlus (Codensity (v :: k -> TYPE rep))
#elif __GLASGOW_HASKELL__ >= 710
instance Alternative v => MonadPlus (Codensity v)
#else
instance MonadPlus v => MonadPlus (Codensity v) where
  mzero = Codensity (\_ -> mzero)
  {-# INLINE mzero #-}
  Codensity m `mplus` Codensity n = Codensity (\k -> m k `mplus` n k)
  {-# INLINE mplus #-}
#endif

-- |
-- This serves as the *left*-inverse (retraction) of 'lift'.
--
--
-- @
-- 'lowerCodensity' . 'lift' ≡ 'id'
-- @
--
-- In general this is not a full 2-sided inverse, merely a retraction, as
-- @'Codensity' m@ is often considerably "larger" than @m@.
--
-- e.g. @'Codensity' ((->) s)) a ~ forall r. (a -> s -> r) -> s -> r@
-- could support a full complement of @'MonadState' s@ actions, while @(->) s@
-- is limited to @'MonadReader' s@ actions.
#if __GLASGOW_HASKELL__ >= 710
lowerCodensity :: Applicative f => Codensity f a -> f a
lowerCodensity a = runCodensity a pure
#else
lowerCodensity :: Monad m => Codensity m a -> m a
lowerCodensity a = runCodensity a return
#endif
{-# INLINE lowerCodensity #-}

-- | The 'Codensity' monad of a right adjoint is isomorphic to the
-- monad obtained from the 'Adjunction'.
--
-- @
-- 'codensityToAdjunction' . 'adjunctionToCodensity' ≡ 'id'
-- 'adjunctionToCodensity' . 'codensityToAdjunction' ≡ 'id'
-- @
codensityToAdjunction :: Adjunction f g => Codensity g a -> g (f a)
codensityToAdjunction r = runCodensity r unit
{-# INLINE codensityToAdjunction #-}

adjunctionToCodensity :: Adjunction f g => g (f a) -> Codensity g a
adjunctionToCodensity f = Codensity (\a -> fmap (rightAdjunct a) f)
{-# INLINE adjunctionToCodensity #-}

-- | The 'Codensity' monad of a representable 'Functor' is isomorphic to the
-- monad obtained from the 'Adjunction' for which that 'Functor' is the right
-- adjoint.
--
-- @
-- 'codensityToComposedRep' . 'composedRepToCodensity' ≡ 'id'
-- 'composedRepToCodensity' . 'codensityToComposedRep' ≡ 'id'
-- @
--
-- @
-- codensityToComposedRep = 'ranToComposedRep' . 'codensityToRan'
-- @

codensityToComposedRep :: Representable u => Codensity u a -> u (Rep u, a)
codensityToComposedRep (Codensity f) = f (\a -> tabulate $ \e -> (e, a))
{-# INLINE codensityToComposedRep #-}

-- |
--
-- @
-- 'composedRepToCodensity' = 'ranToCodensity' . 'composedRepToRan'
-- @
composedRepToCodensity :: Representable u => u (Rep u, a) -> Codensity u a
composedRepToCodensity hfa = Codensity $ \k -> fmap (\(e, a) -> index (k a) e) hfa
{-# INLINE composedRepToCodensity #-}

-- | The 'Codensity' 'Monad' of a 'Functor' @g@ is the right Kan extension ('Ran')
-- of @g@ along itself.
--
-- @
-- 'codensityToRan' . 'ranToCodensity' ≡ 'id'
-- 'ranToCodensity' . 'codensityToRan' ≡ 'id'
-- @
codensityToRan :: Codensity g a -> Ran g g a
codensityToRan (Codensity m) = Ran m
{-# INLINE codensityToRan #-}

ranToCodensity :: Ran g g a -> Codensity g a
ranToCodensity (Ran m) = Codensity m
{-# INLINE ranToCodensity #-}

#ifdef USE_TYPE_IN_TYPE
instance (m ~~ m', Functor f, MonadFree f m')
  => MonadFree f (Codensity (m :: k -> TYPE rep)) where
#else
instance (Functor f, MonadFree f m) => MonadFree f (Codensity m) where
#endif
  wrap t = Codensity (\h -> wrap (fmap (\p -> runCodensity p h) t))
  {-# INLINE wrap #-}

#ifdef USE_TYPE_IN_TYPE
instance (m ~~ m', MonadReader r m')
  => MonadState r (Codensity (m :: k -> TYPE rep)) where
#else
instance MonadReader r m => MonadState r (Codensity m) where
#endif
  get = Codensity (ask >>=)
  {-# INLINE get #-}
  put s = Codensity (\k -> local (const s) (k ()))
  {-# INLINE put #-}

#ifdef USE_TYPE_IN_TYPE
instance (m ~~ m', MonadReader r m')
  => MonadReader r (Codensity (m :: k -> TYPE rep)) where
#else
instance MonadReader r m => MonadReader r (Codensity m) where
#endif
  ask = Codensity (ask >>=)
  {-# INLINE ask #-}
  local f m = Codensity $ \c -> ask >>= \r -> local f . runCodensity m $ local (const r) . c
  {-# INLINE local #-}

-- | Right associate all binds in a computation that generates a free monad
--
-- This can improve the asymptotic efficiency of the result, while preserving
-- semantics.
--
-- See \"Asymptotic Improvement of Computations over Free Monads\" by Janis
-- Voightländer for more information about this combinator.
--
-- <http://www.iai.uni-bonn.de/~jv/mpc08.pdf>
improve :: Functor f => (forall m. MonadFree f m => m a) -> Free f a
improve m = lowerCodensity m
{-# INLINE improve #-}


-- | Wrap the remainder of the 'Codensity' action using the given
-- function.
--
-- This function can be used to register cleanup actions that will be
-- executed at the end.  Example:
--
-- > wrapCodensity (`finally` putStrLn "Done.")
wrapCodensity :: (forall a. m a -> m a) -> Codensity m ()
wrapCodensity f = Codensity (\k -> f (k ()))
