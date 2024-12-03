import Aoc2024.Day1
import Aoc2024.Day2
import Aoc2024.Day3

def main : IO Unit := do
  -- for i in [1:4] do
    -- let part1 := Lean.Name.mkStr3 s!"Day{i}" "Part1" s!"day{i}"
    -- let part2 := Lean.Name.mkStr3 s!"Day{i}" "Part2" s!"day{i}"
    -- dbg_trace s!"{part1}"
    -- dbg_trace s!"{part2}"

    -- IO.println s!"Day{i} Part1: {part1}"
    -- IO.println s!"Day{i} Part2: {part2}"
  IO.println s!"Day1 Part1: {← Day1.Part1.day1}"
  IO.println s!"Day1 Part2: {← Day1.Part2.day1}"
  IO.println s!"Day2 Part1: {← Day2.Part1.day2}"
  IO.println s!"Day2 Part2: {← Day2.Part2.day2}"
  IO.println s!"Day3 Part1: {← Day3.Part1.day3}"
  IO.println s!"Day3 Part2: {← Day3.Part2.day3}"
  return ()

#eval main
