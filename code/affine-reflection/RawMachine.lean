/-
The raw-code extension of ADR-0011. This file is both the executable machine
and its metatheory; acceptance is defined using the actual executable checker.
Primitive types are explicitly instantiated, so no unverified unifier lies
between the checker and the consistency theorem.

Raw data, quotations, and opposed packages are inert until consumed by an
instruction. Checking is structural recursion on a finite input tree. A
checker call is one machine macro-step, not one host CPU operation.
-/
namespace RawReflection

inductive Ty where
  | bot | unit | g | raw | typeCode | bool | opposed
  | arr : Ty → Ty → Ty
  | box : Ty → Ty
  | option : Ty → Ty
  deriving DecidableEq, Repr

inductive Op where
  | i (a : Ty)
  | k (a b : Ty)
  | b (a b c : Ty)
  | c (a b c : Ty)
  | e (a : Ty)
  | d (a : Ty)
  | m (a b : Ty)
  | fold | unfold | star
  | check
  | seal (a : Ty)
  | checkPair | h1
  | ifThen (a : Ty)
  | caseOpt (a b : Ty)
  /-- W can occur in raw input, but the actual checker rejects it as code. -/
  | w (a b : Ty)
  deriving DecidableEq, Repr

inductive Term where
  | atom : Op → Term
  | app : Term → Term → Term
  | quote : Term → Term
  | raw : Term → Term
  | type : Ty → Term
  | tt | ff
  | some : Term → Term
  | none : Ty → Term
  | opposed : Ty → Term → Term → Term
  | junk : Nat → Term
  deriving DecidableEq, Repr

/-- Primitive typing is just this finite table, not a soundness oracle. -/
def opType : Op → Option Ty
  | .i a => some (.arr a a)
  | .k a b => some (.arr a (.arr b a))
  | .b a b c => some (.arr (.arr b c) (.arr (.arr a b) (.arr a c)))
  | .c a b c => some (.arr (.arr a (.arr b c)) (.arr b (.arr a c)))
  | .e a => some (.arr (.box a) a)
  | .d a => some (.arr (.box a) (.box (.box a)))
  | .m a b => some (.arr (.box (.arr a b)) (.arr (.box a) (.box b)))
  | .fold => some (.arr (.arr (.box .g) .bot) .g)
  | .unfold => some (.arr .g (.arr (.box .g) .bot))
  | .star => some .unit
  | .check => some (.arr .raw (.arr .typeCode .bool))
  | .seal a => some (.arr .raw (.option (.box a)))
  | .checkPair => some (.arr .raw (.arr .raw (.arr .typeCode (.option .opposed))))
  | .h1 => some (.arr .opposed .bot)
  | .ifThen a => some (.arr .bool (.arr a (.arr a a)))
  | .caseOpt a b => some (.arr (.option a) (.arr b (.arr (.arr a b) b)))
  | .w _ _ => none

/-- The actual full-language checker. Raw payloads need not type-check.
    Opposed packages do: they record both programs and their runtime type. -/
def infer : Term → Option Ty
  | .atom p => opType p
  | .app f x =>
    match infer f, infer x with
    | some (.arr a b), some a' => if a = a' then some b else none
    | _, _ => none
  | .quote t => (infer t).map Ty.box
  | .raw _ => some .raw
  | .type _ => some .typeCode
  | .tt | .ff => some .bool
  | .some t => (infer t).map Ty.option
  | .none a => some (.option a)
  | .opposed a p q =>
    if infer p = some a ∧ infer q = some (.arr a .bot) then some .opposed else none
  | .junk _ => none

def check (t : Term) (a : Ty) : Bool := decide (infer t = some a)
def Typed (t : Term) (a : Ty) : Prop := infer t = some a

@[simp] theorem check_spec : check t a = true ↔ Typed t a := by
  simp [check, Typed]

@[simp] theorem typed_atom : Typed (.atom p) a ↔ opType p = some a := Iff.rfl

@[simp] theorem typed_app : Typed (.app f x) b ↔
    ∃ a, Typed f (.arr a b) ∧ Typed x a := by
  cases hf : infer f with
  | none => simp [Typed, infer, hf]
  | some ft =>
    cases ft <;> cases hx : infer x <;> simp_all [Typed, infer]

@[simp] theorem typed_quote : Typed (.quote t) b ↔
    ∃ a, b = .box a ∧ Typed t a := by
  cases h : infer t <;> simp [Typed, infer, h, eq_comm]

@[simp] theorem typed_some : Typed (.some t) b ↔
    ∃ a, b = .option a ∧ Typed t a := by
  cases h : infer t <;> simp [Typed, infer, h, eq_comm]

@[simp] theorem typed_raw : Typed (.raw t) a ↔ a = .raw := by
  simp [Typed, infer, eq_comm]
@[simp] theorem typed_type : Typed (.type t) a ↔ a = .typeCode := by
  simp [Typed, infer, eq_comm]
@[simp] theorem typed_tt : Typed .tt a ↔ a = .bool := by
  simp [Typed, infer, eq_comm]
@[simp] theorem typed_ff : Typed .ff a ↔ a = .bool := by
  simp [Typed, infer, eq_comm]
@[simp] theorem typed_none : Typed (.none a) b ↔ b = .option a := by
  simp [Typed, infer, eq_comm]
@[simp] theorem typed_junk : ¬ Typed (.junk n) a := by simp [Typed, infer]

/-- Exact representation of the uniform H1 counterexample package. -/
@[simp] theorem typed_opposed : Typed (.opposed a p q) b ↔
    b = .opposed ∧ Typed p a ∧ Typed q (.arr a .bot) := by
  simp only [Typed, infer]
  split <;> simp_all [eq_comm]

/-- Expanded syntax size, including raw payloads but erasing type annotations. -/
def nodes : Term → Nat
  | .app f x => nodes f + nodes x + 1
  | .quote t | .raw t | .some t => nodes t + 1
  | .opposed _ p q => nodes p + nodes q + 1
  | _ => 1

theorem nodes_pos (t : Term) : 0 < nodes t := by
  cases t <;> simp [nodes] <;> omega

/-- The complete operational relation. Inspection has no consistency premise. -/
inductive Red : Term → Term → Prop where
  | i : Red (.app (.atom (.i a)) x) x
  | k : Red (.app (.app (.atom (.k a b)) x) y) x
  | b : Red (.app (.app (.app (.atom (.b a b c)) f) g) x) (.app f (.app g x))
  | c : Red (.app (.app (.app (.atom (.c a b c)) f) x) y) (.app (.app f y) x)
  | e : Red (.app (.atom (.e a)) (.quote t)) t
  | d : Red (.app (.atom (.d a)) (.quote t)) (.quote (.quote t))
  | m : Red (.app (.app (.atom (.m a b)) (.quote f)) (.quote x)) (.quote (.app f x))
  | unfold : Red (.app (.atom .unfold) (.app (.atom .fold) t)) t
  | check : Red (.app (.app (.atom .check) (.raw p)) (.type a))
      (if check p a then .tt else .ff)
  | seal : Red (.app (.atom (.seal a)) (.raw p))
      (if check p a then .some (.quote p) else .none (.box a))
  | checkPair : Red (.app (.app (.app (.atom .checkPair) (.raw p)) (.raw q)) (.type a))
      (if check p a && check q (.arr a .bot) then .some (.opposed a p q) else .none .opposed)
  | h1 : Red (.app (.atom .h1) (.opposed a p q)) (.app q p)
  | ifTrue : Red (.app (.app (.app (.atom (.ifThen a)) .tt) x) y) x
  | ifFalse : Red (.app (.app (.app (.atom (.ifThen a)) .ff) x) y) y
  | caseNone : Red (.app (.app (.app (.atom (.caseOpt a b)) (.none c)) x) f) x
  | caseSome : Red (.app (.app (.app (.atom (.caseOpt a b)) (.some x)) n) f) (.app f x)
  | left : Red f g → Red (.app f x) (.app g x)
  | right : Red x y → Red (.app f x) (.app f y)
  | insideSome : Red x y → Red (.some x) (.some y)

theorem decrease (r : Red t s) : nodes s < nodes t := by
  induction r <;> simp_all [nodes]
  all_goals first | omega | (split <;> simp [nodes] <;> omega)

/-- The returned proof is erased by compilation; it ties executable steps
    to the specified relation without a second, unverified interpreter. -/
def contract (t : Term) : Option {s : Term // Red t s} :=
  match t with
  | .app (.atom (.i _)) x => some ⟨x, .i⟩
  | .app (.app (.atom (.k _ _)) x) _ => some ⟨x, .k⟩
  | .app (.app (.app (.atom (.b _ _ _)) f) g) x => some ⟨.app f (.app g x), .b⟩
  | .app (.app (.app (.atom (.c _ _ _)) f) x) y => some ⟨.app (.app f y) x, .c⟩
  | .app (.atom (.e _)) (.quote t) => some ⟨t, .e⟩
  | .app (.atom (.d _)) (.quote t) => some ⟨.quote (.quote t), .d⟩
  | .app (.app (.atom (.m _ _)) (.quote f)) (.quote x) => some ⟨.quote (.app f x), .m⟩
  | .app (.atom .unfold) (.app (.atom .fold) t) => some ⟨t, .unfold⟩
  | .app (.app (.atom .check) (.raw p)) (.type a) =>
      some ⟨if check p a then .tt else .ff, .check⟩
  | .app (.atom (.seal a)) (.raw p) =>
      some ⟨if check p a then .some (.quote p) else .none (.box a), .seal⟩
  | .app (.app (.app (.atom .checkPair) (.raw p)) (.raw q)) (.type a) =>
      some ⟨if check p a && check q (.arr a .bot) then
        .some (.opposed a p q) else .none .opposed, .checkPair⟩
  | .app (.atom .h1) (.opposed _ p q) => some ⟨.app q p, .h1⟩
  | .app (.app (.app (.atom (.ifThen _)) .tt) x) _ => some ⟨x, .ifTrue⟩
  | .app (.app (.app (.atom (.ifThen _)) .ff) _) y => some ⟨y, .ifFalse⟩
  | .app (.app (.app (.atom (.caseOpt _ _)) (.none _)) x) _ => some ⟨x, .caseNone⟩
  | .app (.app (.app (.atom (.caseOpt _ _)) (.some x)) _) f => some ⟨.app f x, .caseSome⟩
  | _ => none

/-- Leftmost outermost execution, with code/data payloads inert. -/
def step (t : Term) : Option {s : Term // Red t s} :=
  match contract t with
  | some r => some r
  | none =>
    match t with
    | .app f x =>
      match step f with
      | some ⟨g, h⟩ => some ⟨.app g x, .left h⟩
      | none =>
        match step x with
        | some ⟨y, h⟩ => some ⟨.app f y, .right h⟩
        | none => none
    | .some x =>
      match step x with
      | some ⟨y, h⟩ => some ⟨.some y, .insideSome h⟩
      | none => none
    | _ => none

theorem step_decrease (_h : step t = some r) : nodes r.val < nodes t :=
  decrease r.property

/-- A total evaluator for arbitrary raw inputs, without fuel or an oracle. -/
def run (t : Term) : Term :=
  match step t with
  | none => t
  | some ⟨s, _⟩ => run s
termination_by nodes t
decreasing_by exact decrease ‹Red t s›

/-- All primitive and contextual steps preserve acceptance by the actual
    checker. The checker cases use successful *local* checks, never H0/H1. -/
theorem preservation (r : Red t s) (ht : Typed t a) : Typed s a := by
  induction r generalizing a with
  | i =>
    simp [opType] at ht
    obtain ⟨_, ⟨rfl, rfl⟩, hx⟩ := ht
    exact hx
  | k =>
    simp [opType] at ht
    obtain ⟨_, ⟨_, ⟨rfl, rfl, rfl⟩, hx⟩, _⟩ := ht
    exact hx
  | b =>
    simp [opType] at ht ⊢
    obtain ⟨_, ⟨_, ⟨_, ⟨rfl, rfl, rfl, rfl⟩, hf⟩, hg⟩, hx⟩ := ht
    exact ⟨_, hf, _, hg, hx⟩
  | c =>
    simp [opType] at ht ⊢
    obtain ⟨_, ⟨_, ⟨_, ⟨rfl, rfl, rfl, rfl⟩, hf⟩, hx⟩, hy⟩ := ht
    exact ⟨_, ⟨_, hf, hy⟩, hx⟩
  | e =>
    simp [opType] at ht
    obtain ⟨_, ⟨rfl, rfl⟩, ⟨_, heq, hb⟩⟩ := ht
    cases heq
    exact hb
  | d =>
    simp [opType] at ht ⊢
    obtain ⟨_, ⟨rfl, rfl⟩, ⟨_, heq, hb⟩⟩ := ht
    cases heq
    exact ⟨_, rfl, _, rfl, hb⟩
  | m =>
    simp [opType] at ht ⊢
    obtain ⟨_, ⟨_, ⟨rfl, rfl, rfl⟩, ⟨_, heq, hf⟩⟩, ⟨_, heq2, hx⟩⟩ := ht
    cases heq
    cases heq2
    exact ⟨_, rfl, _, hf, hx⟩
  | unfold =>
    simp [opType] at ht
    obtain ⟨_, ⟨rfl, rfl⟩, ⟨_, ⟨rfl, _⟩, hb⟩⟩ := ht
    exact hb
  | check =>
    simp [opType] at ht
    subst a
    split <;> simp
  | «seal» =>
    simp [opType] at ht
    subst a
    split <;> simp_all
  | checkPair =>
    simp [opType] at ht
    subst a
    split <;> simp_all
  | h1 =>
    simp [opType] at ht ⊢
    obtain ⟨_, ⟨rfl, rfl⟩, _, hp, hq⟩ := ht
    exact ⟨_, hq, hp⟩
  | ifTrue =>
    simp [opType] at ht
    obtain ⟨_, ⟨_, ⟨rfl, rfl, rfl⟩, hx⟩, _⟩ := ht
    exact hx
  | ifFalse =>
    simp [opType] at ht
    obtain ⟨_, ⟨_, ⟨rfl, rfl, rfl⟩, _⟩, hy⟩ := ht
    exact hy
  | caseNone =>
    simp [opType] at ht
    obtain ⟨_, ⟨_, ⟨_, rfl, rfl, rfl⟩, hx⟩, _⟩ := ht
    exact hx
  | caseSome =>
    simp [opType] at ht ⊢
    obtain ⟨_, ⟨_, ⟨_, ⟨rfl, rfl, rfl, rfl⟩, ⟨_, heq, hx⟩⟩, _⟩, hf⟩ := ht
    cases heq
    exact ⟨_, hf, hx⟩
  | left r ih =>
    obtain ⟨b, hf, hx⟩ := typed_app.mp ht
    exact typed_app.mpr ⟨b, ih hf, hx⟩
  | right r ih =>
    obtain ⟨b, hf, hx⟩ := typed_app.mp ht
    exact typed_app.mpr ⟨b, hf, ih hx⟩
  | insideSome r ih =>
    obtain ⟨b, rfl, hb⟩ := typed_some.mp ht
    exact typed_some.mpr ⟨b, rfl, ih hb⟩

theorem step_preservation (ht : Typed t a) (_h : step t = some r) : Typed r.val a :=
  preservation r.property ht

inductive Value : Term → Prop where
  | atom (p) : Value (.atom p)
  | quote (t) : Value (.quote t)
  | raw (t) : Value (.raw t)
  | type (a) : Value (.type a)
  | tt : Value .tt
  | ff : Value .ff
  | some : Value x → Value (.some x)
  | none (a) : Value (.none a)
  | opposed (a p q) : Value (.opposed a p q)
  | k1 : Value x → Value (.app (.atom (.k a b)) x)
  | b1 : Value x → Value (.app (.atom (.b a b c)) x)
  | b2 : Value x → Value y → Value (.app (.app (.atom (.b a b c)) x) y)
  | c1 : Value x → Value (.app (.atom (.c a b c)) x)
  | c2 : Value x → Value y → Value (.app (.app (.atom (.c a b c)) x) y)
  | m1 : Value x → Value (.app (.atom (.m a b)) x)
  | f1 : Value x → Value (.app (.atom .fold) x)
  | check1 : Value x → Value (.app (.atom .check) x)
  | pair1 : Value x → Value (.app (.atom .checkPair) x)
  | pair2 : Value x → Value y → Value (.app (.app (.atom .checkPair) x) y)
  | if1 : Value x → Value (.app (.atom (.ifThen a)) x)
  | if2 : Value x → Value y → Value (.app (.app (.atom (.ifThen a)) x) y)
  | case1 : Value x → Value (.app (.atom (.caseOpt a b)) x)
  | case2 : Value x → Value y → Value (.app (.app (.atom (.caseOpt a b)) x) y)

theorem value_not_bot (v : Value t) : ¬ Typed t .bot := by
  cases v with
  | atom p => cases p <;> simp [opType]
  | _ => simp [opType]

theorem value_box (v : Value t) (ht : Typed t (.box a)) :
    ∃ s, t = .quote s ∧ Typed s a := by
  cases v with
  | atom p => cases p <;> simp [opType] at ht
  | _ => simp_all [opType]

theorem value_g (v : Value t) (ht : Typed t .g) :
    ∃ s, t = .app (.atom .fold) s ∧ Typed s (.arr (.box .g) .bot) := by
  cases v with
  | atom p => cases p <;> simp [opType] at ht
  | _ => simp_all [opType]

theorem value_raw (v : Value t) (ht : Typed t .raw) : ∃ s, t = .raw s := by
  cases v with
  | atom p => cases p <;> simp [opType] at ht
  | _ => simp_all [opType]

theorem value_type (v : Value t) (ht : Typed t .typeCode) : ∃ a, t = .type a := by
  cases v with
  | atom p => cases p <;> simp [opType] at ht
  | _ => simp_all [opType]

theorem value_bool (v : Value t) (ht : Typed t .bool) : t = .tt ∨ t = .ff := by
  cases v with
  | atom p => cases p <;> simp [opType] at ht
  | _ => simp_all [opType]

theorem value_option (v : Value t) (ht : Typed t (.option a)) :
    t = .none a ∨ ∃ s, t = .some s ∧ Typed s a := by
  cases v with
  | atom p => cases p <;> simp [opType] at ht
  | _ => simp_all [opType]

theorem value_opposed (v : Value t) (ht : Typed t .opposed) :
    ∃ a p q, t = .opposed a p q ∧ Typed p a ∧ Typed q (.arr a .bot) := by
  cases v with
  | atom p => cases p <;> simp [opType] at ht
  | opposed a p q => exact ⟨a, p, q, rfl, (typed_opposed.mp ht).2⟩
  | _ => simp_all [opType]

theorem value_application (vf : Value f) (vx : Value x)
    (ht : Typed (.app f x) a) : Value (.app f x) ∨ ∃ s, Red (.app f x) s := by
  cases vf with
  | atom p =>
    cases p with
    | i => exact .inr ⟨x, .i⟩
    | k => exact .inl (.k1 vx)
    | b => exact .inl (.b1 vx)
    | c => exact .inl (.c1 vx)
    | m => exact .inl (.m1 vx)
    | fold => exact .inl (.f1 vx)
    | check => exact .inl (.check1 vx)
    | checkPair => exact .inl (.pair1 vx)
    | ifThen => exact .inl (.if1 vx)
    | caseOpt => exact .inl (.case1 vx)
    | e =>
      simp [opType] at ht
      obtain ⟨_, ⟨rfl, rfl⟩, hx⟩ := ht
      obtain ⟨s, rfl, _⟩ := value_box vx hx
      exact .inr ⟨s, .e⟩
    | d =>
      simp [opType] at ht
      obtain ⟨_, ⟨rfl, rfl⟩, hx⟩ := ht
      obtain ⟨s, rfl, _⟩ := value_box vx hx
      exact .inr ⟨.quote (.quote s), .d⟩
    | unfold =>
      simp [opType] at ht
      obtain ⟨_, ⟨rfl, rfl⟩, hx⟩ := ht
      obtain ⟨s, rfl, _⟩ := value_g vx hx
      exact .inr ⟨s, .unfold⟩
    | «seal» =>
      simp [opType] at ht
      obtain ⟨_, ⟨rfl, rfl⟩, hx⟩ := ht
      obtain ⟨s, rfl⟩ := value_raw vx hx
      exact .inr ⟨_, .seal⟩
    | h1 =>
      simp [opType] at ht
      obtain ⟨_, ⟨rfl, rfl⟩, hx⟩ := ht
      obtain ⟨b, p, q, rfl, _, _⟩ := value_opposed vx hx
      exact .inr ⟨.app q p, .h1⟩
    | star => simp [opType] at ht
    | w => simp [opType] at ht
  | k1 => exact .inr ⟨_, .k⟩
  | b1 vy => exact .inl (.b2 vy vx)
  | b2 => exact .inr ⟨_, .b⟩
  | c1 vy => exact .inl (.c2 vy vx)
  | c2 => exact .inr ⟨_, .c⟩
  | m1 vy =>
    simp [opType] at ht
    obtain ⟨_, ⟨_, ⟨rfl, rfl, rfl⟩, hf⟩, hx⟩ := ht
    obtain ⟨s, rfl, _⟩ := value_box vy hf
    obtain ⟨t, rfl, _⟩ := value_box vx hx
    exact .inr ⟨.quote (.app s t), .m⟩
  | check1 vy =>
    simp [opType] at ht
    obtain ⟨_, ⟨_, ⟨rfl, rfl, rfl⟩, hy⟩, hx⟩ := ht
    obtain ⟨p, rfl⟩ := value_raw vy hy
    obtain ⟨b, rfl⟩ := value_type vx hx
    exact .inr ⟨_, .check⟩
  | pair1 vy => exact .inl (.pair2 vy vx)
  | pair2 vp vq =>
    simp [opType] at ht
    obtain ⟨_, ⟨_, ⟨_, ⟨rfl, rfl, rfl, rfl⟩, hp⟩, hq⟩, hx⟩ := ht
    obtain ⟨p, rfl⟩ := value_raw vp hp
    obtain ⟨q, rfl⟩ := value_raw vq hq
    obtain ⟨b, rfl⟩ := value_type vx hx
    exact .inr ⟨_, .checkPair⟩
  | if1 vy => exact .inl (.if2 vy vx)
  | if2 vb vy =>
    simp [opType] at ht
    obtain ⟨_, ⟨_, ⟨_, ⟨rfl, rfl, rfl, rfl⟩, hb⟩, _⟩, _⟩ := ht
    rcases value_bool vb hb with rfl | rfl
    · exact .inr ⟨_, .ifTrue⟩
    · exact .inr ⟨_, .ifFalse⟩
  | case1 vy => exact .inl (.case2 vy vx)
  | case2 vo vn =>
    simp [opType] at ht
    obtain ⟨_, ⟨_, ⟨_, ⟨rfl, rfl, rfl, rfl⟩, ho⟩, _⟩, _⟩ := ht
    rcases value_option vo ho with rfl | ⟨s, rfl, _⟩
    · exact .inr ⟨_, .caseNone⟩
    · exact .inr ⟨_, .caseSome⟩
  | _ => simp [opType] at ht

theorem progress (ht : Typed t a) : Value t ∨ ∃ s, Red t s := by
  induction t generalizing a with
  | atom p => exact .inl (.atom p)
  | quote t => exact .inl (.quote t)
  | raw t => exact .inl (.raw t)
  | type b => exact .inl (.type b)
  | tt => exact .inl .tt
  | ff => exact .inl .ff
  | none b => exact .inl (.none b)
  | opposed b p q => exact .inl (.opposed b p q)
  | junk n => exact (typed_junk ht).elim
  | app f x ihf ihx =>
    obtain ⟨b, hf, hx⟩ := typed_app.mp ht
    rcases ihf hf with vf | ⟨f', rf⟩
    · rcases ihx hx with vx | ⟨x', rx⟩
      · exact value_application vf vx (typed_app.mpr ⟨b, hf, hx⟩)
      · exact .inr ⟨.app f x', .right rx⟩
    · exact .inr ⟨.app f' x, .left rf⟩
  | some t ih =>
    obtain ⟨b, rfl, hb⟩ := typed_some.mp ht
    rcases ih hb with vb | ⟨s, r⟩
    · exact .inl (.some vb)
    · exact .inr ⟨.some s, .insideSome r⟩

/-- No term accepted at Bot by this actual, full-language checker exists. -/
theorem consistency (t : Term) (ht : Typed t .bot) : False := by
  rcases progress ht with hv | ⟨s, hr⟩
  · exact value_not_bot hv ht
  · exact consistency s (preservation hr ht)
termination_by nodes t
decreasing_by exact decrease hr

theorem checker_h0 (p : Term) : check p .bot = false := by
  cases h : check p .bot with
  | false => rfl
  | true => exact (consistency p (check_spec.mp h)).elim

/-- One theorem quantifies over all runtime type codes and both raw inputs. -/
theorem checker_h1 (a : Ty) (p q : Term) :
    (check p a && check q (.arr a .bot)) = false := by
  cases h : check p a && check q (.arr a .bot) with
  | false => rfl
  | true =>
    have h' : Typed p a ∧ Typed q (.arr a .bot) := by simpa using h
    exact (consistency (.app q p)
      (typed_app.mpr ⟨a, h'.2, h'.1⟩)).elim

theorem h0_internal : check (.atom (.e .bot)) (.arr (.box .bot) .bot) = true := rfl
theorem h1_internal : check (.atom .h1) (.arr .opposed .bot) = true := rfl

/-- Acceptance of a literal certificate is exactly acceptance of its payload. -/
theorem box_representation (p : Term) (a : Ty) :
    check (.quote p) (.box a) = check p a := by
  cases h : infer p <;> simp [check, infer, h]

/-- This is the dependent package over *all* finite type codes, not a
    fixed-type approximation to the H1 counterexample relation. -/
theorem opposed_representation (a : Ty) (p q : Term) :
    check (.opposed a p q) .opposed = (check p a && check q (.arr a .bot)) := by
  change decide ((if infer p = some a ∧ infer q = some (.arr a .bot)
    then some Ty.opposed else none) = some Ty.opposed) = _
  split
  · rename_i h
    change true = (check p a && check q (.arr a .bot))
    rw [check_spec.mpr h.1, check_spec.mpr h.2]
    rfl
  · rename_i h
    change false = (check p a && check q (.arr a .bot))
    cases hp : check p a with
    | false => rfl
    | true =>
      cases hq : check q (.arr a .bot) with
      | false => rfl
      | true => exact (h ⟨check_spec.mp hp, check_spec.mp hq⟩).elim

theorem run_preservation (t : Term) (ht : Typed t a) : Typed (run t) a := by
  rw [run]
  split
  · exact ht
  · exact run_preservation _ (preservation ‹Red t _› ht)
termination_by nodes t
decreasing_by exact decrease ‹Red t _›

/-- The implemented strategy cannot miss every applicable reduction. -/
theorem step_complete (r : Red t s) : step t ≠ none := by
  induction r with
  | left r ih =>
    rw [step]
    split
    · simp
    · cases h : step _ with
      | none => exact (ih h).elim
      | some out => simp
  | right r ih =>
    rw [step]
    split
    · simp
    · cases hf : step _ with
      | some out => simp
      | none =>
        cases hx : step _ with
        | none => exact (ih hx).elim
        | some out => simp
  | insideSome r ih =>
    rw [step]
    simp only [contract]
    cases h : step _ with
    | none => exact (ih h).elim
    | some out => simp
  | _ => simp [step, contract]

theorem run_stops (t : Term) : step (run t) = none := by
  rw [run]
  split
  · assumption
  · exact run_stops _
termination_by nodes t
decreasing_by exact decrease ‹Red t _›

theorem run_value (ht : Typed t a) : Value (run t) := by
  rcases progress (run_preservation t ht) with hv | ⟨_, hr⟩
  · exact hv
  · exact (step_complete hr (run_stops t)).elim

/-- Agreement of the internal checker instruction with the actual checker,
    for arbitrary raw input, not just the self-check demonstration. -/
theorem check_exec (p : Term) (a : Ty) :
    run (.app (.app (.atom .check) (.raw p)) (.type a)) =
      (if check p a then .tt else .ff) := by
  cases h : check p a <;> simp [run, step, contract, h]

theorem seal_exec (p : Term) (a : Ty) :
    run (.app (.atom (.seal a)) (.raw p)) =
      (if check p a then .some (.quote p) else .none (.box a)) := by
  cases h : check p a <;> simp [run, step, contract, h]

theorem pair_exec (p q : Term) (a : Ty) :
    run (.app (.app (.app (.atom .checkPair) (.raw p)) (.raw q)) (.type a)) =
      (if check p a && check q (.arr a .bot) then .some (.opposed a p q)
        else .none .opposed) := by
  cases h : check p a && check q (.arr a .bot) <;> simp [run, step, contract, h]

theorem h1_exec (a : Ty) (p q : Term) :
    run (.app (.atom .h1) (.opposed a p q)) = run (.app q p) := by
  conv => lhs; rw [run]
  rfl

/-- The logical reading of the package representations. These are ordinary
    finite witnesses to checker acceptance; they contain no semantic oracle. -/
def Certificate (a : Ty) := {p : Term // check p a = true}
def OpposedWitness := (a : Ty) × (Certificate a × Certificate (.arr a .bot))

theorem h0_reading : (Certificate .bot → False) ↔
    (∀ p, check p .bot = true → False) := by
  constructor
  · intro f p hp
    exact f ⟨p, hp⟩
  · intro f p
    exact f p.val p.property

theorem h1_reading : (OpposedWitness → False) ↔
    (∀ a p q, check p a = true → check q (.arr a .bot) = true → False) := by
  constructor
  · intro f a p q hp hq
    exact f ⟨a, ⟨p, hp⟩, ⟨q, hq⟩⟩
  · intro f w
    exact f w.fst w.snd.fst.val w.snd.snd.val w.snd.fst.property w.snd.snd.property

end RawReflection
