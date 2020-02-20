{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Control.Monad.Cell.Class where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Control.Monad.Cell.Class (MonadCell (..))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Holmes (Merge (..), Result (..))
import Data.Kind (Type)
import Data.Monoid (Ap (..))
import Data.Primitive.MutVar (MutVar)
import qualified Data.Primitive.MutVar as MutVar
import Prelude hiding (read)

-- | A monad for testing. 'Lestrade' is only capable of single computation
-- branches, and can't backtrack.
newtype Lestrade (h :: Type) (x :: Type)
  = Lestrade { unwrap :: MaybeT (ST h) x }
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , PrimMonad
    )
  deriving (Semigroup, Monoid)
    via (Ap (Lestrade h) x)

scotlandYardSays :: (forall h. Lestrade h x) -> Maybe x
scotlandYardSays xs = runST (runMaybeT (unwrap xs))

instance MonadCell (Lestrade h) where
  data Cell (Lestrade h) x = Cell
    ( MutVar (PrimState (Lestrade h)) (x, Lestrade h ())
    )

  discard = empty

  fill x = do
    ref <- MutVar.newMutVar (x, mempty)
    pure (Cell ref)

  watch cell@(Cell ref) f = do
    (content, callback) <- MutVar.readMutVar ref

    MutVar.writeMutVar ref (content, callback *> with cell f)
    with cell f

  with cell f = read cell >>= f

  write (Cell ref) news = do
    (content, callback) <- MutVar.readMutVar ref

    case content <<- news of
      Unchanged -> pure ()
      Changed x -> MutVar.writeMutVar ref (x, callback) *> callback
      Failure   -> discard

read :: Cell (Lestrade h) x -> Lestrade h x
read (Cell ref) = do
  (content, _) <- MutVar.readMutVar ref

  pure content
