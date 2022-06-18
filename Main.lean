import Dta

open Dta

def main : IO Unit := do
  let arr ← IO.FS.readBinFile "data/test.dta"
  header
  |>.parse arr
  |> repr
  |> IO.println

#eval main