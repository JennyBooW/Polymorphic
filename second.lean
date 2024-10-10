namespace Positive

inductive Positive : Type where
  | one  : Positive
  | succ : Positive → Positive

-- Positive + --------------------------------

def Positive.plus : Positive → Positive → Positive
  | Positive.one, k    => Positive.succ k
  | Positive.succ n, k => Positive.succ (n.plus k)

instance : Add Positive where
  add := Positive.plus

----------------------------------------------

-- Positive * --------------------------------

def Positive.mul : Positive → Positive → Positive
  | Positive.one, k    => k
  | Positive.succ n, k => n.mul k + k

instance : Mul Positive where
  mul := Positive.mul

----------------------------------------------

-- To ℕ --------------------------------------

instance : OfNat Positive (n + 1) where
  ofNat :=
    let rec natPlusOne : Nat → Positive
    | 0     => Positive.one
    | k + 1 => Positive.succ (natPlusOne k)
  natPlusOne n

def Positive.toNat : Positive → Nat
  | Positive.one => 1
  | Positive.succ n => n.toNat + 1


instance : ToString Positive where
  toString x := toString (x.toNat)

end Positive


open Positive

-- Heterogeneous add

class HPlus (α : Type) (β : Type) (γ : outParam Type) where
  hPlus : α → β → γ

def addNatPos : Nat → Positive → Positive
  | 0, p     => p
  | n + 1, p => Positive.succ (addNatPos n p)

def addPosNat : Positive → Nat → Positive
  | p, 0 => p
  | p, n + 1 => Positive.succ (addPosNat p n)

instance : HPlus Nat Positive Positive where
  hPlus := addNatPos

instance : HPlus Positive Nat Positive where
  hPlus := addPosNat

#eval HPlus.hPlus (3 : Positive) (5 : Nat)

instance [Add α] : HPlus α α α where
  hPlus := Add.add

#eval HPlus.hPlus (1 : Nat) (10 : Nat)
