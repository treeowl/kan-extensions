{-# LANGUAGE TypeFamilies, TypeInType, TypeOperators, ConstraintKinds #-}

module Control.Monad.Codensity.Internal
  (
    type ($$$)
  , type HeterogeneousApply
  ) where
import Data.Kind

-- Writing instances like
-- instance MonadFail f => MonadFail (Codensity f)
-- leads to some hidden flexible instances. Haddock will show things like
--
-- MonadFail f => MonadFail (Codensity * LiftedRep f)
-- 
-- Since FlexibleInstances are bad for inference, we avoid them when
-- we can by carefully pushing kind constraints to the left. Unfortunately,
-- this is a slightly delicate operation, and leads to ugly-looking
-- constraints.

infixr 9 $$$
-- | Apply a class to a type that is not known, a priory, to
-- have the correct kind.
type (f :: x -> Constraint) $$$ (a :: x') =
  (x ~ x', HeterogeneousApply f a)

type family HeterogeneousApply (f :: x -> y) (a :: x') :: y where
  HeterogeneousApply (f :: x -> y) (a :: x) = f a
