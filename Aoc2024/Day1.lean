import Aoc2024.Basic

namespace Day1

/-- Parse a single line into a pair of numbers. -/
def parseLine (line : String) : Nat × Nat :=
  let nums := line.splitOn " "
  let (left, right) := (nums.head!.toNat!, nums.getLast!.toNat!)
  (left, right)

/-- Parse the input into an array of pairs. -/
def parseLines (input : String) : Array (Nat × Nat) :=
  input.trim.splitOn "\n" |>.map parseLine |>.toArray

/-- Parse both columns into separate arrays. -/
def parseInput (input : String) : Array Nat × Array Nat :=
  (parseLines input).unzip

namespace Part1

/-- Calculate the total distance between two arrays of numbers. -/
def totalDistance (left right : Array Nat) : Nat :=
  let sortedLeft : Array Int := left.qsort (· < ·) |>.map .ofNat
  let sortedRight : Array Int := right.qsort (· < ·) |>.map .ofNat
  -- Cast to int to avoid saturating subtraction
  sortedLeft.zip sortedRight |>.map (fun (l,r) => (l-r).natAbs) |>.sum

/-- Solution for part 1-/
def solve (input : String) : Nat := Id.run do
  let (left, right) := parseInput input
  return totalDistance left right

def day1 (input : String := "input/day1.txt") : IO Nat := do
  return solve (← IO.FS.readFile input)

end Part1

namespace Part2

/-- Calculate the similarity score between two arrays of numbers. -/
def similarityScore (left right : Array Nat) : Nat :=
  left.foldl (fun acc l =>
    let occurrences := right.filter (·==l) |>.size
    acc + l * occurrences
  ) 0

/-- Solution for part 2 -/
def solve (input : String) : Nat := Id.run do
  let (left, right) := parseInput input
  return similarityScore left right

def day1 (input : String := "input/day1.txt") : IO Nat := do
  return solve (← IO.FS.readFile input)

end Part2

end Day1

