/- Red/green acceptance tests for the full raw-code machine. -/
import RawMachine
open RawReflection

def checkTy : Ty := .arr .raw (.arr .typeCode .bool)
def h0Ty : Ty := .arr (.box .bot) .bot
def h1Ty : Ty := .arr .opposed .bot

example : infer (.raw (.junk 17)) = some .raw := by decide
example : check (.junk 17) .bot = false := by decide
example : check (.atom (.w .g .bot)) (.arr .g .bot) = false := by decide
example : check (.raw (.atom (.w .g .bot))) .raw = true := by decide
example : check (.atom .check) checkTy = true := by decide
example : check (.atom (.e .bot)) h0Ty = true := by decide
example : check (.atom .h1) h1Ty = true := by decide
example : check (.quote (.atom .h1)) (.box h1Ty) = true := by decide
example : run (.app (.app (.atom .check) (.raw (.atom .check))) (.type checkTy)) = .tt := by simp [run, step, contract, check, infer, opType, checkTy]
example : run (.app (.app (.atom .check) (.raw (.junk 3))) (.type .unit)) = .ff := by simp [run, step, contract, check, infer]
example : run (.app (.atom (.seal .unit)) (.raw (.atom .star))) =
    .some (.quote (.atom .star)) := by simp [run, step, contract, check, infer, opType]
example : run (.app (.atom (.seal .bot)) (.raw (.atom .star))) =
    .none (.box .bot) := by simp [run, step, contract, check, infer, opType]
example : run (.app (.app (.app (.atom .checkPair)
    (.raw (.atom .star))) (.raw (.atom (.i .unit)))) (.type .unit)) =
    .none .opposed := by simp [run, step, contract, check, infer, opType]

/-- A sealed program is actually run in an object-language option branch. -/
def useSeal : Term :=
  .app (.app (.app (.atom (.caseOpt (.box .unit) .unit))
    (.app (.atom (.seal .unit)) (.raw (.app (.atom (.i .unit)) (.atom .star)))))
    (.atom .star)) (.atom (.e .unit))

example : check useSeal .unit = true := by decide
example : run useSeal = .atom .star := by simp [useSeal, run, step, contract, check, infer, opType]

-- Every raw-machine root rule is exercised. Some empty-type redex shapes
-- deliberately use ill-typed data, to test execution independently of typing.
example : run (.app (.atom (.i .unit)) (.atom .star)) = .atom .star := by
  simp [run, step, contract]
example : run (.app (.app (.atom (.k .unit .raw)) (.atom .star)) (.raw (.junk 1))) =
    .atom .star := by simp [run, step, contract]
example : run (.app (.app (.app (.atom (.b .unit .unit .unit))
    (.atom (.i .unit))) (.atom (.i .unit))) (.atom .star)) = .atom .star := by
  simp [run, step, contract]
example : run (.app (.app (.app (.atom (.c .bool .bool .bool))
    (.atom (.k .bool .bool))) .tt) .ff) = .ff := by simp [run, step, contract]
example : run (.app (.atom (.d .unit)) (.quote (.atom .star))) =
    .quote (.quote (.atom .star)) := by simp [run, step, contract]
example : run (.app (.app (.atom (.m .unit .unit)) (.quote (.atom (.i .unit))))
    (.quote (.atom .star))) = .quote (.app (.atom (.i .unit)) (.atom .star)) := by
  simp [run, step, contract]
example : run (.app (.atom .unfold) (.app (.atom .fold) (.atom (.i .unit)))) =
    .atom (.i .unit) := by simp [run, step, contract]
example : check (.opposed .unit (.atom .star) (.atom (.i .unit))) .opposed = false := by decide
example : run (.app (.atom .h1) (.opposed .unit (.atom .star) (.atom (.i .unit)))) =
    .atom .star := by simp [run, step, contract]
example : run (.app (.app (.app (.atom (.ifThen .bool)) .tt) .ff) .tt) = .ff := by
  simp [run, step, contract]
example : run (.app (.app (.app (.atom (.ifThen .bool)) .ff) .ff) .tt) = .tt := by
  simp [run, step, contract]
example : run (.app (.app (.app (.atom (.caseOpt .unit .bool)) (.none .unit)) .tt)
    (.app (.atom (.k .bool .unit)) .ff)) = .tt := by simp [run, step, contract]
example : run (.some (.app (.atom (.i .unit)) (.atom .star))) =
    .some (.atom .star) := by simp [run, step, contract]
example : run (.quote (.app (.atom (.i .unit)) (.atom .star))) =
    .quote (.app (.atom (.i .unit)) (.atom .star)) := by simp [run, step, contract]
example : run (.raw (.junk 19)) = .raw (.junk 19) := by simp [run, step, contract]

def sealedH1 : Term := .app (.atom (.seal h1Ty)) (.raw (.atom .h1))
example : run sealedH1 = .some (.quote (.atom .h1)) := by
  simp [sealedH1, h1Ty, run, step, contract, check, infer, opType]

/-- The successful branch consumes sealed checker code and invokes it on H1.
    Both occurrences of C rearrange arguments; neither duplicates the code. -/
def callCheckerOnH1 : Term :=
  .app (.app (.atom (.c (.box checkTy) .typeCode .bool))
    (.app (.app (.atom (.c (.box checkTy) .raw (.arr .typeCode .bool)))
      (.atom (.e checkTy))) (.raw (.atom .h1)))) (.type h1Ty)

def checkedChecker : Term :=
  .app (.app (.app (.atom (.caseOpt (.box checkTy) .bool))
    (.app (.atom (.seal checkTy)) (.raw (.atom .check)))) .ff) callCheckerOnH1

example : check checkedChecker .bool = true := by decide
example : run checkedChecker = .tt := by
  simp [checkedChecker, callCheckerOnH1, checkTy, h1Ty, run, step, contract, check, infer, opType]

-- These propositions concern arbitrary raw inputs and runtime type codes.
example (p : Term) : check p .bot = false := checker_h0 p
example (a : Ty) (p q : Term) :
    (check p a && check q (.arr a .bot)) = false := checker_h1 a p q

#eval run (.app (.app (.atom .check) (.raw (.atom .check))) (.type checkTy))
#eval run useSeal
#eval run sealedH1
#eval run checkedChecker
#print axioms consistency
#print axioms checker_h0
#print axioms checker_h1
#print axioms step_decrease
#print axioms step_preservation
#print axioms run_value
#print axioms box_representation
#print axioms opposed_representation
#print axioms h0_reading
#print axioms h1_reading
#print axioms check_exec
#print axioms seal_exec
#print axioms pair_exec
#print axioms h1_exec
