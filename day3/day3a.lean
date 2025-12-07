def toDigit (c: Char) : Nat := Char.toNat c - 48

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
  | [] => if not (s2 == []) then [List.reverse s2] else []
  | x :: xs => if x == c then
    (butLast $ List.reverse s2) :: split c xs []
    else split c xs (x :: s2)

def indexMax' (xs: List Char) (i : Nat) (j : Nat) (m : Char) : Prod Nat Nat :=
  match xs with
  | [] => (toDigit m, i)
  | x :: ys => if toDigit x > toDigit m then indexMax' ys j (j + 1) x
  else indexMax' ys i (j + 1) m

def indexMax (xs: List Char) : Prod Nat Nat :=
  indexMax' xs 0 0 '0'

def maxJoltage (xs : List Char) : Nat :=
  let (m, i) := indexMax xs;
  if i == List.length xs - 1
    then 10 * Prod.fst (indexMax (butLast xs)) + m
    else match (List.map toDigit (xs.drop (i+1))).max? with
    | some x => (10 * m) + x
    | none => 0

def main : IO Unit := do
  let inp <- IO.FS.readFile "day3/input.txt"
  let s := String.toList inp
  IO.println $ sum $ List.map maxJoltage $ split '\n' s []

#eval main
