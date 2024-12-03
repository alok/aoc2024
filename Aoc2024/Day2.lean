import Aoc2024.Basic

namespace Day2

/-- Parse a line of space-separated numbers -/
def parseLine (line : String) : Array Nat :=
  line.splitOn " "
    |>.map String.toNat?
    |>.filter (·.isSome)
    |>.map (·.get!)
    |>.toArray

/-- A sequence of numbers is safe if:
1. All numbers are either increasing or decreasing
2. Adjacent numbers differ by 1-3 -/
def safe? (nums : Array Nat) : Bool := Id.run do
  if nums.size < 2 then return true

  -- Check first two numbers to determine if increasing or decreasing
  let isIncreasing := nums = nums.qsort (· < ·)
  let isDecreasing := nums = nums.qsort (· > ·)
  if !isIncreasing && !isDecreasing then return false

  -- Check each adjacent pair
  for i in [1:nums.size] do
    let curr := nums[i]!
    let prev := nums[i-1]!
    let diff := if curr > prev then curr - prev else prev - curr

    -- Fail if difference not in [1,3] or direction changes
    if diff < 1 || diff > 3 then return false
    if isIncreasing && curr < prev then return false
    if !isIncreasing && curr > prev then return false

  return true

namespace Part1

/-- Count safe sequences in input -/
def solve (input : String) : Nat :=
  input.splitOn "\n"
    |>.filter (·.length > 0)
    |>.map parseLine
    |>.filter safe?
    |>.length

def day2 (input : String := "input/day2.txt") : IO Nat := do
  return solve (← IO.FS.readFile input)

end Part1

namespace Part2

/-- Try removing each number once and check if sequence becomes safe -/
def safeWithDampener? (nums : Array Nat) : Bool := Id.run do
  -- First check if already safe
  if safe? nums then return true

  -- Try removing each number
  for i in [0:nums.size] do
    -- Create new array without element at index i
    let withoutI := nums.toList.enum.filter (fun (j, _) => i ≠ j) |>.map Prod.snd |>.toArray
    if safe? withoutI then return true

  return false

/-- Count sequences that are safe with Problem Dampener -/
def solve (input : String) : Nat :=
  input.splitOn "\n"
    |>.filter (·.length > 0)
    |>.map parseLine
    |>.filter safeWithDampener?
    |>.length

def day2 (input : String := "input/day2.txt") : IO Nat := do
  return solve (← IO.FS.readFile input)



end Part2

end Day2

#eval Day2.Part1.day2
#eval Day2.Part2.day2
