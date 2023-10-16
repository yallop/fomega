  $ dune exec -- fomega ../examples/bools.f
  Bool :: *
  true : ∀α.α → α → α
  false : ∀α.α → α → α
  if : Bool → Bool
  not : Bool → Bool
  and : Bool → Bool → Bool
  or : Bool → Bool → Bool
  $ dune exec -- fomega ../examples/bools_via_sums.f
  Unit :: *
  Bool :: *
  if : Bool → (∀A.A → A → A)
  unit : ∀A.A → A
  true : (∀A.A→A) + (∀A.A → A)
  false : (∀A.A→A) + (∀A.A → A)
  not : Bool → Bool
  and : Bool → Bool → Bool
  or : Bool → Bool → Bool
  $ dune exec -- fomega ../examples/bools_with_existentials.f
  Unit :: *
  unit : ∀A.A → A
  bool_package : ∃β.β×(β×(β→(∀α.α→α→α)))
  (inl [Unit] unit) : Unit + Unit
  $ dune exec -- fomega ../examples/equality.f
  Eq :: * ⇒ * ⇒ *
  refl : ∀A.∀P::*⇒*.P A → P A
  symm : ∀A.∀B.(∀P::*⇒*.P A→P B) → Eq B A
  trans : ∀A.∀B.∀C.Eq A B → Eq B C → Eq A C
  lift : ∀A.∀B.∀P::*⇒*.Eq A B → Eq (P A) (P B)
  $ dune exec -- fomega ../examples/hoas.f
  ExpDict :: (*⇒*) ⇒ *
  abs : ∀A.∀B.∀E::*⇒*.ExpDict E →
                              (E A→E B) → E (A→B)
  app : ∀A.∀B.∀E::*⇒*.ExpDict E →
                              E (A→B) → E A → E B
  Expression :: * ⇒ *
  evalDict : ExpDict (λA.A)
  eval : ∀A.Expression A → A
  Nat :: *
  zero : Nat
  succ : Nat → Nat
  add : Nat → Nat → Nat
  sizeDict : ExpDict (λA.Nat)
  size : ∀A.Expression A → Nat
  $ dune exec -- fomega ../examples/lists.f
  List :: * ⇒ *
  nil : ∀α.∀φ::*⇒*.φ α → (α→φ α→φ α) → φ α
  cons : ∀α.α →
               List α →
               (∀φ::*⇒*.φ α →
                             (α→φ α→φ α) → φ α)
  fold : ∀α.∀B.(α→B→B) → B → List α → B
  append : ∀α.List α → List α → List α
  concat : ∀α.List (List α) → List α
  $ dune exec -- fomega ../examples/nats.f
  Nat :: *
  zero : ∀α.α → (α→α) → α
  succ : Nat → (∀α.α → (α→α) → α)
  add : Nat → Nat → Nat
  pred : Nat → Nat
  $ dune exec -- fomega ../examples/nested_types.f
  Perfect :: * ⇒ *
  zeroP : ∀A.A →
               (∀T::*⇒*.(∀A'.A'→T A') →
                            (∀A'.T (A'×A')→T A') → T A)
  succP : ∀A.Perfect (A×A) →
               (∀T::*⇒*.(∀A'.A'→T A') →
                            (∀B.T (B×B)→T B) → T A)
  $ dune exec -- fomega ../examples/pairs.f
  Pair :: * ⇒ * ⇒ *
  pair : ∀A.∀B.A → B → (∀C.(A→B→C) → C)
  fst : ∀A.∀B.Pair A B → A
  snd : ∀A.∀B.Pair A B → B
  $ dune exec -- fomega ../examples/simply-typed.f
  Base :: *
  id : Base → Base
  idf : (Base→Base) → Base → Base
  compose : (Base→Base) → (Base→Base) → Base → Base
  $ dune exec -- fomega ../examples/sums.f
  Sum :: * ⇒ * ⇒ *
  _inl : ∀A.∀B.A → (∀C.(A→C) → (B→C) → C)
  _inr : ∀A.∀B.B → (∀C.(A→C) → (B→C) → C)
  _case : ∀A.∀B.Sum A B → Sum A B
  bimap : ∀A.∀B.∀C.(A→C) → (B→C) → Sum A B → C
  swap : ∀A.∀B.Sum A B → Sum B A
  proj : ∀A.∀B.Sum A A → A
