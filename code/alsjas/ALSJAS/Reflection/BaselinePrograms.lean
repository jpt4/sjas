import ALSJAS.Reflection.Programs

/-!
# Baseline reflective artifacts

These are the exact checker-facing artifacts promised by the ALSJAS plan. Their
native acceptance is an implementation result only. It is not used as evidence
for consistency; the structural preservation theorem is a separate module and
completion gate.
-/

namespace ALSJAS.Reflection.BaselinePrograms

open ALSJAS

/-- Type of the selected fixed-point fold direction. -/
def foldType (system : SystemRef) : Ty :=
  .lolli (.box system (.lolli (.godel system) .zero)) (.godel system)

/-- Closed fixed-point fold program. -/
def fold (system : SystemRef) : Term :=
  .lam (.box system (.lolli (.godel system) .zero))
    (.godelFold system (.var 0))

def foldCertificate (system : System.Sealed) : SExpr :=
  Checker.certificateSExpr system (foldType system.reference)
    (fold system.reference)

/-- Type of the selected fixed-point unfold direction. -/
def unfoldType (system : SystemRef) : Ty :=
  .lolli (.godel system) (.box system (.lolli (.godel system) .zero))

/-- Closed fixed-point unfold program. -/
def unfold (system : SystemRef) : Term :=
  .lam (.godel system) (.godelUnfold system (.var 0))

def unfoldCertificate (system : System.Sealed) : SExpr :=
  Checker.certificateSExpr system (unfoldType system.reference)
    (unfold system.reference)

/-- Complete certificate for the distinguished native consistency program. -/
def self0Certificate (system : System.Sealed) : SExpr :=
  Checker.certificateSExpr system (Programs.self0Type system.reference)
    system.self0

/-- Level-1 conflict collapse type. -/
def collapseType (system : SystemRef) : Ty :=
  .lolli (.clash1 system) (.box system .zero)

def collapseCertificate (system : System.Sealed) : SExpr :=
  Checker.certificateSExpr system (collapseType system.reference)
    system.collapse1

/-- Derived Level-1 consistency type. -/
def self1Type (system : SystemRef) : Ty :=
  .lolli (.clash1 system) .zero

/-- Compose native conflict collapse with the exact `self0`. Each input is used
once; no contraction or hidden proof search occurs. -/
def self1 (system : System.Sealed) : Term :=
  .lam (.clash1 system.reference)
    (.app system.self0 (.app system.collapse1 (.var 0)))

def self1Certificate (system : System.Sealed) : SExpr :=
  Checker.certificateSExpr system (self1Type system.reference) (self1 system)

end ALSJAS.Reflection.BaselinePrograms
