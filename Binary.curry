{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Binary where

-- ---------------------------------------------------------------------------
-- Nat
-- ---------------------------------------------------------------------------

-- Algebraic data type to represent natural numbers
data Nat = IHi | O Nat | I Nat
  deriving Eq

-- successor, O(n)
succ :: Nat -> Nat
succ IHi    = O IHi        -- 1       + 1 = 2
succ (O bs) = I bs         -- 2*n     + 1 = 2*n + 1
succ (I bs) = O (succ bs)  -- 2*n + 1 + 1 = 2*(n+1)

-- ---------------------------------------------------------------------------
-- Integer
-- ---------------------------------------------------------------------------

-- Algebraic data type to represent integers
data BinInt = Neg Nat | Zero | Pos Nat
  deriving Eq