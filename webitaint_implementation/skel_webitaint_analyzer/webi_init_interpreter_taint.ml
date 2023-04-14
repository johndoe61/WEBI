module L = List
module P = Printf
open L
open Monads
open Wtypes
open Types_interpreter
open ClientLanguage
open ServerLanguage
open DeviceLanguageTaint
open Client_interpreter
open Server_interpreter
open Device_interpreter_taint
open Webi_taint
open Webi_interpreter_taint
open Webi_init_taint

exception Fail of string

module MWI = Monads.Rand(Monads.ID)
module T = Types_interpreter.T_Interp
module S = Server_interpreter.SR_Interp
module C = Client_interpreter.CL_Interp
module D = Device_interpreter_taint.DV_Interp
module W = Webi_interpreter_taint.WEBI_Interp

module rec WIT : sig

end = WIT

module WI_Input = struct
  include Unspec(MW)(WIT)(T)(S)(D)(C)(W)

  let sprinkler_open = "sprinkler_open"
  let sprinkler_close = "sprinkler_close"
  let sprinkler = "sprinkler"
  let sprinkler_status = "sprinkler_status"
  let moisture_sensor = "moisture_sensor"
  let moisture = "moisture"
  let empty_str = ""
  let virtual_assistant = "virtual_assistant"
  let living_room = "living_room"
  let lightOnPlease = "lightOnPlease"
  let lightOn = "lightOn"
  let command = "command"
  let waterplant = "waterplant"
  let valve_status = "valve_status"
  let valve_open = "valve_open"
  let valve_close = "valve_close"
  let valve_location = "valve"
  let valve = "valve"
  let sodium_sensor_status = "sodium_sensor_status"
  let sodium_sensor = "sodium_sensor"
  let sodium_dispenser = "sodium_dispenser"
  let dispenser_status = "dispenser_status"
  let add_sodium = "add_sodium"
  let replenish_sodium = "replenish_sodium"
  let _100 = 100
  let _4 = 4
  let _6 = 6
  let _X = "X"
  let thermometer = "thermometer"
  let switch = "switch"
  let motion_sensor = "motion_sensor"
  let window_close = "window_close"
  let window_open = "window_open"
  let window_status = "window_status"
  let switch_status = "switch_status"
  let window = "window"
  let temp = "temp"
  let oven_turn_on = "oven_turn_on"
  let oven_turn_off = "oven_turn_off"
  let oven_status = "oven_status"
  let oven = "oven"
  let kitchen = "kitchen"
  let setLevel_80 = "setLevel_80"
  let setLevel_signalPattern = "setLevel_signalPattern"
  let setLevel_0 = "setLevel_0"
  let motion_status = "motion_status"
  let _no_one_home = "_no one home"
  let house = "house"
  let strobe = "strobe"
  let stop = "stop"
  let smoke_status = "smoke_status"
  let smoke_sensor = "smoke_sensor"
  let phone_status = "phone_status"
  let phone = "phone"
  let alarm_status = "alarm_status"
  let alarm = "alarm"
  let _not_calling = "not calling"
  let take_photo = "take_photo"
  let send_media = "send_media"
  let record_video = "record_video"
  let camera_status = "camera_status"
  let _video = "video"
  let _sending_media = "sending_media"
  let _photo = "photo"
  let _empty = "empty"
  let _hang = "hang"
  let _calling = "calling"
  let _call = "call"
  let camera = "camera"
  let _27 = 27
  let _24 = 24
  let _80 = 80
  let _40 = 80
  let _0 = 0
  let _1 = 1
end

module WEBI_Init_Interp = MakeInterpreter(WI_Input)(Client_interpreter.CL_Interp)(Device_interpreter_taint.DV_Interp)(Server_interpreter.SR_Interp)(Webi_interpreter_taint.WEBI_Interp)(Types_interpreter.T_Interp)

open WI_Input
open WEBI_Init_Interp
