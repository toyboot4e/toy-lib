{-# LANGUAGE BangPatterns #-}

-- | Manhattan distance
--
-- = Typical problems
--
-- - [Typical 036 - Max Manhattan Distance (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_aj)
-- - [Typical 070 - Plant Planning (★4)](https://atcoder.jp/contests/typical90/tasks/typical90_br)

module Math.Manhattan where

-- | Rotates @(x, y)@ pair in 45 degree and scales \(\sqrt 2\). Note that @x@ comes first in the
-- pair.
--
-- 2D Manhattan distance is definned as \(\Delta x + \Delta y\). After this transformation, it's
-- calculated with \(\Delta x + \Delta y\).
--
-- This transformation applies
-- \(\frac {1} {\sqrt 2} \begin{bmatrix} 1 && -1 \\ 1 && 1 \end{bmatrix} \),
-- but without the cofficient.
--
-- = Typical problems
--
-- - [Typical 036 - Max Manhattan Distance (★5)](https://atcoder.jp/contests/typical90/tasks/typical90_aj)
rot45 :: (Int, Int) -> (Int, Int)
rot45 (!x, !y) = (x - y, x + y)

