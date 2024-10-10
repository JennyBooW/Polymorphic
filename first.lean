def List.sum [Add α] [OfNat α 0] : List α → α
  | []      => 0
  | x :: xs => x + xs.sum

def nums : List Nat := [1, 2, 3, 4]

#eval nums.sum

structure Point (α : Type) where
  x : α
  y : α
deriving Repr

instance [Add α] : Add (Point α) where
  add p1 p2 := { x := p1.x + p2.x, y := p1.y + p2.y }

def A : Point Nat := { x := 10, y := 10}

#eval A + A
