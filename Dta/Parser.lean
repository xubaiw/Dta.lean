import Dta.Basic
import Parsec

open Parsec ByteArrayParser

namespace Dta

def release : ByteArrayParser Release := do
  match Release.fromString? (←readString 3) with
  | some r => return r
  | _      => error "invalid <release> field"

def byteorder : ByteArrayParser Byteorder := do
  match Byteorder.fromString? (←readString 3) with
  | some r => return r
  | _      => error "invalid <byteorder> field"

def read16 : Byteorder → ByteArrayParser UInt16
| .lsf => read16LE
| .msf => read16BE

def read32 : Byteorder → ByteArrayParser UInt32
| .lsf => read32LE
| .msf => read32BE

def read64 : Byteorder → ByteArrayParser UInt64
| .lsf => read64LE
| .msf => read64BE

def k (r : Release) (e : Byteorder) : ByteArrayParser (typeK r) := by
  cases r <;> simp [typeK]
  repeat exact read16 e
  exact read32 e

def n (e : Byteorder) := read64 e

def label (e : Byteorder) : ByteArrayParser String := do
  let length ← read16 e
  readString length.toNat

def timestamp? : ByteArrayParser (Option String) := do
  let length ← read8
  match length with
  |  0 => return none
  | 17 => return (←readString 17)
  |  _ => error "length of timestamp must be 0 or 17"

def header : ByteArrayParser Header := do
  expectS "<stata_dta>"
  expectS "<header>"
  expectS "<release>"
  let r ← release
  expectS "</release>"
  expectS "<byteorder>"
  let e ← byteorder
  expectS "</byteorder>"
  expectS "<K>"
  let k ← k r e
  expectS "</K>"
  expectS "<N>"
  let n ← n e
  expectS "</N>"
  expectS "<label>"
  let label ← label e
  expectS "</label>"
  expectS "<timestamp>"
  let timestamp? ← timestamp?
  expectS "</timestamp>"
  expectS "</header>"
  -- expectS "</stata_dta>"
  return ⟨r, e, k, n, label, timestamp?⟩

end Dta