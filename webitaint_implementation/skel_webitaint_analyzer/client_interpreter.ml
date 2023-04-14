open Monads
open Wtypes
open Types_interpreter
open ClientLanguage

exception Fail of string

module TC = Types_interpreter.T_Interp
module MC = Monads.Rand(Monads.ID)

module rec CL_Types : sig

end = CL_Types

module CL_Input = struct
  include Unspec(MC)(CL_Types)(TC)

end

module CL_Interp = MakeInterpreter(CL_Input)(TC)
open CL_Interp
