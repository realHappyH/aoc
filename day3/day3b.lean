def toDigit (c: Char) : Nat := c.toNat - 48

def sum : List Nat -> Nat
 | [] => 0
 | x :: xs => x + sum xs

def butLast (xs : List α) : List α :=
  match xs with
  | [] => []
  | x :: [_] => [x]
  | x :: ys => x :: butLast ys

def split (c : Char) (s1 : List Char) (s2 : List Char) : List (List Char) :=
  match s1 with
  | [] => if not (s2 = []) then [s2.reverse] else []
  | x :: xs => if x = c then
    (butLast $ s2.reverse) :: split c xs []
    else split c xs (x :: s2)

def indexMax' (xs: List Char) (i : Nat) (j : Nat) (m : Char) : Prod Nat Nat :=
  match xs with
  | [] => (toDigit m, i)
  | x :: ys => if toDigit x > toDigit m then indexMax' ys j (j + 1) x
  else indexMax' ys i (j + 1) m

def indexMax (xs: List Char) : Prod Nat Nat :=
  indexMax' xs 0 0 '0'

def maxJoltage (n : Nat) (xs : List Char) : Nat :=
  match n with
    | 0 => 0
    | n =>
      let (m, i) := indexMax xs;
      let k := xs.length - i;
      10^(min k n) * maxJoltage (n - k) (xs.toSlice 0 i).toList + maxJoltage (min (k) n-1) (xs.toSlice (i+1) (xs.length + 1)).toList + m * 10^(min (k-1) (n-1))
-- proof that maxJoltage terminates (lean doesn't like that it's recursive)
termination_by n -- I want to prove that n eventually decreases to 0
decreasing_by
all_goals simp_wf
.calc n - (xs.length - i)       ≤ n - 1 := by sorry -- for that I would have to prove these inequalities
      n - 1                     < n     := by sorry -- but I have no idea how to prove things in lean so I'm skipping it
.calc min (xs.length - i) n - 1 ≤ n - 1 := by grind -- this is just the outline of how it probably would go
      n - 1                     < n     := by sorry

def main : IO Unit := do
  let inp <- IO.FS.readFile "day3/input.txt"
  let s := inp.toList
  IO.println $ sum $ (split '\n' s []).map (maxJoltage 12)

#eval! main -- lean complains that I did not prove that maxJoltage always terminates, so I have to force it to evaluate
