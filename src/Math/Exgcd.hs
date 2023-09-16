-- | Extended Eucried Algorithm
--
-- = Typical problems
-- - [ABC 186 E - Throne](https://atcoder.jp/contests/abc186/tasks/abc186_e)
--
-- = Related
-- - [Water drawing problem](https://science-log.com/%E3%83%9B%E3%83%BC%E3%83%A0%E3%83%9A%E3%83%BC%E3%82%B8/%E6%95%B0%E5%AD%A6top%E3%83%9A%E3%83%BC%E3%82%B8/%E6%95%B4%E6%95%B0top/%E6%95%B4%E6%95%B0%E7%AC%AC%EF%BC%93%E7%AB%A0%E7%9B%AE%E6%AC%A1/%E6%95%B4%E6%95%B0%E7%AC%AC%EF%BC%93%E7%AB%A0%E7%AC%AC%EF%BC%91%E7%AF%80/%E6%95%B4%E6%95%B0%E7%AC%AC%EF%BC%93%E7%AB%A0%E7%AC%AC%EF%BC%91%E7%AF%80%EF%BD%83-%EF%BC%91/%E5%95%8F%E9%A1%8Cc003/)

module Math.Exgcd where

-- | @4tsuzuru <https://zenn.dev/link/comments/29d659a57ead56>
--
-- @exgcd a b@ returns @(g, na, nb)@ where \(g = a \cdot n_a + b \cdot n_b\).
--
-- >>> exgcd 3 5
-- (1,2,-1)
-- >>> 1 == 3 * 2 + 5 * (-1)
-- True
--
-- Note that \(n_{a} = a^{-1} \mod b\):
-- \[
-- \begin{aligned}
-- n_a a + n_b b &= 1 \\
-- \Rightarrow n_a a &\equiv 1 \mod b \\
-- \Leftrightarrow n_a &\equiv a^{-1} \mod b
-- \end{aligned}
-- \]
exgcd :: Integral a => a -> a -> (a, a, a)
exgcd a b = f $ go a b 1 0 0 1
  where
    go r0 r1 s0 s1 t0 t1
      | r1 == 0 = (r0, s0, t0)
      | otherwise = go r1 r2 s1 s2 t1 t2
      where
        (!q, !r2) = r0 `divMod` r1
        s2 = s0 - q * s1
        t2 = t0 - q * t1
    f (!g, !u, !v)
      | g < 0 = (-g, -u, -v)
      | otherwise = (g, u, v)

-- | Example for showing how to use `exgcd` to calculate the modular multicative inverse.
invModGcd :: Integral a => a -> a -> Maybe a
invModGcd a m = case exgcd a m of
  (1, na, _) -> Just na
  _ -> Nothing

