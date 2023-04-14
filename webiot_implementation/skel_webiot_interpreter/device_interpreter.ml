open Monads
open Wtypes
open Types_interpreter
open DeviceLanguage

exception Fail of string


module TD = Types_interpreter.T_Interp
module MD = Monads.Rand(Monads.ID)

module rec DV_Types : sig

end = DV_Types

module DV_Input = struct
  include Unspec(MD)(DV_Types)(TD)

end

module DV_Interp = MakeInterpreter(DV_Input)(TD)
open DV_Interp
