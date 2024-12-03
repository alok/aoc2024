import Std.Internal.Parsec
open Std Internal Parsec String

namespace Day3
namespace Part1
/-- Represents a multiplication instruction with two numbers -/
structure MulInstr where
  x : Nat
  y : Nat
  deriving Repr

/-- Parse a single digit -/
def digit : Parser Char := satisfy (·.isDigit)

/-- Parse a number of 1-3 digits -/
def number : Parser Nat := do
  let d1 ← digit
  let d2? ← optional digit
  let d3? ← optional digit
  let digits := [d1] ++
    (match d2? with | some d => [d] | none => []) ++
    (match d3? with | some d => [d] | none => [])
  return (String.mk digits).toNat!

/-- Parse a valid mul instruction of form mul(x,y) where x,y are 1-3 digit numbers -/
def mulInstr : Parser MulInstr := do
  let _ ← pstring "mul("
  let x ← number
  let _ ← pchar ','
  let y ← number
  let _ ← pchar ')'
  return { x := x, y := y }

/-- Try to parse a mul instruction, return none if invalid -/
def tryMulInstr (s : String) : Option MulInstr :=
  match Parser.run mulInstr s with
  | .ok instr => some instr
  | .error _ => none

/-- Evaluate a mul instruction to get its result -/
def MulInstr.eval (instr : MulInstr) : Nat :=
  instr.x * instr.y

/-- Find all valid mul instructions in a string -/
def findMulInstrs (input : String) : List MulInstr := Id.run do
  let chars := input.data
  let mut instrs := []
  let mut i := 0
  while h: i < chars.length do
    if hh: i + 3 < chars.length then
      let m := chars[i]
      let u := chars[i+1]
      let l := chars[i+2]
      if (m == 'm' && u == 'u' && l == 'l') then
        -- Try to parse a mul instruction starting at this position
        let rest := String.mk (chars.drop i)
        match tryMulInstr rest with
        | some instr =>
          instrs := instr :: instrs
          i := i + 1
        | none => i := i + 1
      else
        i := i + 1
    else
      i := i + 1
  return instrs.reverse

/-- Solve part 1: sum all multiplication results -/
def solve (input : String) : Nat :=
  let instrs := findMulInstrs input
  instrs.map MulInstr.eval |>.sum

def day3 (input : String := "input/day3.txt") : IO Nat := do
  return solve (← IO.FS.readFile input)

#eval day3

end Part1

namespace Part2

/-- Represents a control instruction that enables/disables mul instructions -/
inductive ControlInstr where
  /-- Enable mul instructions -/
  | «do()»    : ControlInstr
  /-- Disable mul instructions -/
  | «don't()» : ControlInstr
  deriving Repr

/-- Parse a control instruction -/
def controlInstr : Parser ControlInstr := do
  let ctrl ← (pstring "do()" *> pure .«do()») <|>
            (pstring "don't()" *> pure .«don't()»)
  return ctrl

/-- Try to parse a control instruction, return none if invalid -/
def tryControlInstr (s : String) : Option ControlInstr :=
  match Parser.run controlInstr s with
  | .ok ctrl => some ctrl
  | .error _ => none

/-- Represents an instruction that can be either mul or control -/
inductive Instruction where
  | mul  (instr : Part1.MulInstr)
  | ctrl (instr : ControlInstr)
  deriving Repr

/-- Find all valid instructions (mul and control) in a string -/
def findInstructions (input : String) : List Instruction := Id.run do
  let chars := input.data
  let mut instrs := []
  let mut i := 0
  while h: i < chars.length do
    if hh: i + 3 < chars.length then
      let c1 := chars[i]
      let c2 := chars[i+1]
      let c3 := chars[i+2]
      if (c1 == 'm' && c2 == 'u' && c3 == 'l') then
        -- Try to parse a mul instruction
        let rest := String.mk (chars.drop i)
        match Part1.tryMulInstr rest with
        | some instr =>
          instrs := Instruction.mul instr :: instrs
          i := i + 1
        | none => i := i + 1
      else if (c1 == 'd' && c2 == 'o') then
        -- Try to parse a control instruction
        let rest := String.mk (chars.drop i)
        match tryControlInstr rest with
        | some ctrl =>
          instrs := Instruction.ctrl ctrl :: instrs
          i := i + 1
        | none => i := i + 1
      else
        i := i + 1
    else
      i := i + 1
  return instrs.reverse

/-- Evaluate instructions considering enable/disable state -/
def evalInstructions (instrs : List Instruction) : Nat := Id.run do
  let mut sum := 0
  let mut enabled := true  -- Initially enabled

  for instr in instrs do
    match instr with
    | .mul m =>
      if enabled then
        sum := sum + m.eval
    | .ctrl c =>
      match c with
      | .«do()» => enabled := true
      | .«don't()» => enabled := false

  return sum

/-- Solve part 2: sum enabled multiplication results -/
def solve (input : String) : Nat :=
  let instrs := findInstructions input
  evalInstructions instrs

def day3 (input : String := "input/day3.txt") : IO Nat := do
  return solve (← IO.FS.readFile input)

#eval day3

end Part2

end Day3