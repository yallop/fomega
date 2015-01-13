# Nat :: *
Nat = ∀α::*.α → (α → α) → α;

# succ : Nat
zero = Λα::*.λz:α.λs:α → α.z;

# succ : Nat → Nat
succ = λn:Nat.Λα::*.λz:α.λs:α → α.s (n [α] z s);

# add : Nat → Nat → Nat
add = λm:Nat.λn:Nat.m [Nat] n succ;

# pred : Nat → Nat
pred =
  λn:Nat. π₁ (n [Nat×Nat] 〈zero, zero〉 (λp:Nat×Nat.〈π₂ p, succ (π₂ p)〉));

