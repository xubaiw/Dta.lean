namespace Dta


/-- Stata .dta specification version -/
inductive Release
| /-- Stata  8 -/ «113»
| /-- Stata 10 -/ «114»
| /-- Stata 12 -/ «115»
| /-- Stata 13 -/ «117»
| /-- Stata 14 - 17 -/ «118»
| /-- Stata 15 - 17 (when dataset has more than 32,767 variables) -/ «119»
deriving DecidableEq, Repr

namespace Release

def toString : Release → String
| «113» => "113"
| «114» => "114"
| «115» => "115"
| «117» => "117"
| «118» => "118"
| «119» => "119"

def fromString? : String → Option Release
| "113" => «113»
| "114" => «114»
| "115" => «115»
| "117" => «117»
| "118" => «118»
| "119" => «119»
| _     => none

instance : ToString Release := ⟨ toString ⟩

end Release

/-- Byteorder of .dta file -/
inductive Byteorder
|  /-- Most Significant byte First -/ msf
| /-- Least Significant byte First -/ lsf
deriving Repr

namespace Byteorder

def toString : Byteorder → String
| lsf => "LSF"
| msf => "MSF"

def fromString? : String → Option Byteorder
| "LSF" => lsf
| "MSF" => msf
| _     => none

instance : ToString Byteorder := ⟨ toString ⟩

end Byteorder

def typeK : Release → Type
| .«119» => UInt32
| _      => UInt16

namespace typeK

instance : Repr (typeK r) where
  reprPrec k _ := by
    cases r
    <;> simp [typeK] at k
    <;> exact repr k

end typeK

structure Header where
  release : Release
  byteorder : Byteorder
  k : typeK release
  n : UInt64
  label : String
  timestamp : Option String
  deriving Repr

end Dta