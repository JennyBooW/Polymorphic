open Positive

inductive Positive : Type where
  | one  : Positive
  | succ : Positive → Positive

def Positive.plus : Positive → Positive → Positive
  | Positive.one, k    => Positive.succ k
  | Positive.succ n, k => Positive.succ (n.plus k)

instance : Add Positive where
  add := Positive.plus

end Positive
