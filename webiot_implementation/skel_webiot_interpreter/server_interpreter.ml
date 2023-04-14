open Monads
open Wtypes
open Types_interpreter
open ServerLanguage

exception Fail of string

module MS = Monads.Rand(Monads.ID)
module TS = Types_interpreter.T_Interp

module rec SR_Types : sig

end = SR_Types

module SR_Input = struct
  include Unspec(MS)(SR_Types)(TS)
  let _act = "statement evaluation : SERVER act\n"
  let _get = "statement evaluation : SERVER get\n"
  let _wait = "statement evaluation : SERVER wait\n"
  let _wrong_branch = "statemente evaluation : SERVER wait but wrong branch\n"
end

module SR_Interp = MakeInterpreter(SR_Input)(TS)
open SR_Interp
