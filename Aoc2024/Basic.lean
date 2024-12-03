import Std.Internal.Parsec

open Std.Internal.Parsec

-- abbrev LineParser := Std.Internal.Parsec Unit (Array Nat)
-- def parseSpaceSepNums : LineParser := do
--   let nums ← many (do
--     let n ← many1Chars digit
--     skipChar ' '? -- Optional space after number
--     pure n.toNat!
--   )
--   pure nums.toArray

def _root_.Array.sum [Add α] [Zero α] (a : Array α) : α :=
  a.foldr (· + ·) 0
