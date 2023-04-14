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

exception Fail of string

module MW = Monads.Rand(Monads.ID)
module T = Types_interpreter.T_Interp
module S = Server_interpreter.SR_Interp
module C = Client_interpreter.CL_Interp
module D = Device_interpreter_taint.DV_Interp

module rec WT : sig

end = WT

module W_Input = struct
  include Unspec(MW)(WT)(T)(S)(D)(C)

let _serviceInit = "current WEBI step : serviceInit\n"
let _serverStep = "current WEBI step : serverStep\n"
let _retService = "current WEBI step : retService\n"
let _retServiceBoot = "current WEBI step : retServiceBoot\n"
let _retServiceDiscard = "current WEBI step : retServiceDiscard\n"
let _clientStep = "current WEBI step : clientStep\n"
let _clientCall = "current WEBI step : clientCall\n"
let _deviceSensor = "current WEBI step : deviceSensor\n"
let _deviceActuator = "current WEBI step : deviceActuator\n"
let _deviceReading = "current WEBI step : deviceReading\n"
let _webServices = "creating a web service\n"
let _thatsallfolks = "that's all folks!!!\n"
let _run = "current step : run\n"
let _ss_a = "adding to : _ss_a\n"
let _ss_r = "adding to : _ss_r\n"
let _ss_g = "adding to : _ss_g\n"
let _ss_e = "adding to : _ss_e\n"
let _cc_r = "adding to : _cc_r\n"
let _cc_t = "adding to : _cc_t\n"
let _cc_c = "adding to : _cc_c\n"
let _cc_e = "adding to : _cc_e\n"
let _cc_boot = "adding to : _cc_boot\n"
let pp_printing_Error_conf c = MW.ret (Printf.fprintf c "This execution returns ERROR!\n")
let pp_printing_Ok_conf c = MW.ret (Printf.fprintf c "This execution is Ok!\n")
let pp_printing_result c = MW.ret (Printf.fprintf c "Execution Summary:\n\n")
let pp_printing_sink_reached c = MW.ret (Printf.fprintf c "The tainted sink has ")
let pp_printing_trace c = MW.ret (Printf.fprintf c "Execution Trace =\n")
let pp_serverStep c = MT.ret (fun h -> MT.ret (fun i -> MT.ret (fun j -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "ServiceStep => ID : (%n,%n), HOST : %s, TAINTED : %s, TIME : %n\n" i j h (match t with | T.Taint -> "yes" | T.Untaint -> "no") time))))))
let pp_retService c = MT.ret (fun u ->  MT.ret (fun i -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "RetService => ID : %n, URL : %s, TAINTED : %s, TIME : %n\n" i u (match t with | T.Taint -> "yes" | T.Untaint -> "no") time)))))
let pp_retServiceDiscard c = MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "RetServiceDiscard => TAINTED : %s\n, TIME : %n" (match t with | T.Taint -> "yes" | T.Untaint -> "no") time)))
let pp_retServiceBoot c = MT.ret (fun u ->  MT.ret (fun i -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "RetServiceBoot => ID : %n, URL : %s, TAINTED : %s, TIME : %n\n" i u (match t with | T.Taint -> "yes" | T.Untaint -> "no") time)))))
let pp_serviceInit c = MT.ret (fun i ->  MT.ret (fun u -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "ServiceInit => ID : %n, URL : %s, TAINTED : %s, TIME : %n\n" i u (match t with | T.Taint -> "yes" | T.Untaint -> "no") time)))))
let pp_clientCallUnsafe c = MT.ret (fun i -> MT.ret (fun u -> MT.ret (fun j -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "ClientCallUnsafe => ID : %n, URL : %s, SID : %n, TAINTED : %s, TIME : %n\n" i u j (match t with | T.Taint -> "yes" | T.Untaint -> "no") time))))))
let pp_clientCallSafe c = MT.ret (fun i -> MT.ret (fun u -> MT.ret (fun j -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "ClientCallSafe => ID : %n, URL : %s, SID : %n, TAINTED : %s, TIME : %n\n" i u j (match t with | T.Taint -> "yes" | T.Untaint -> "no") time))))))
let pp_run c = MT.ret (fun u -> MT.ret (fun i -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "Run => ID : %n, URL : %s, TAINTED : %s, TIME : %n\n" i u (match t with | T.Taint -> "yes" | T.Untaint -> "no") time)))))
let pp_clientStep c = MT.ret (fun u -> MT.ret (fun i -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "ClientStep => ID : %n, URL : %s, TAINTED : %s, TIME : %n\n" i u (match t with | T.Taint -> "yes" | T.Untaint -> "no") time)))))
let string_of_logEvent le = (match le with | T.Log_Act (a,(d,l),t) -> String.concat " " ["ACT";a;"DEVICE_NAME :";d;"LOCATION";l;"TIME";(string_of_int t)]
                                           | T.Log_PE (((d,l),T.Std v),t) -> String.concat " " ["PE";d;l;(match v with
                                                                                                          | T.Bool b -> (match b with | T -> "true" | F -> "false")
                                                                                                          | T.Nat n -> (string_of_int n)
                                                                                                          | T.Str s -> s
                                                                                                          | T.Unit -> "()"
                                                                                                          | T.Undefined -> "undefined" );"TIME";(string_of_int t)]
                                           | T.Log_PE (((d,l),T.ExtValue _),t) -> "EXTVALUE")
let string_of_std_val v = (match v with
                           | T.Std v' -> ((match v' with
                                           | T.Bool b -> (match b with | T -> "true" | F -> "false")
                                           | T.Nat n -> (string_of_int n)
                                           | T.Str s -> s
                                           | T.Unit -> "()"
                                           | T.Undefined -> "undefined" ))
                           | T.ExtValue _ -> "EXTVALUE")
let pp_deviceSensor c = MT.ret (fun le -> MT.ret (fun time -> MT.ret (Printf.fprintf c "DeviceSensor => EVENT : %s, TIME : %n\n" (string_of_logEvent le) time)))
let pp_deviceActuatorSafe (c:T.out_chan) = MT.ret (fun le -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "DeviceActuatorSafe => EVENT : %s, TAINTED : %s, TIME : %n\n" (string_of_logEvent le) (match t with | T.Taint -> "yes" | T.Untaint -> "no") time))))
let pp_deviceActuatorUnsafe (c:T.out_chan) = MT.ret (fun le -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "DeviceActuatorUnsafe => EVENT : %s, TAINTED : %s, TIME : %n\n" (string_of_logEvent le) (match t with | T.Taint -> "yes" | T.Untaint -> "no") time))))
let pp_deviceReading (c:T.out_chan) = MT.ret (fun i -> MT.ret (fun j -> MT.ret (fun (d,l) -> MT.ret (fun (v : D.dv_val) -> MT.ret (fun t -> MT.ret (fun time -> MT.ret (Printf.fprintf c "DeviceReading => ID : %n, SID : %n, DEVICE_ID : (%s,%s), VALUE : %s, TAINTED : %s, TIME : %n\n" i j d l (string_of_std_val v) (match t with | T.Taint -> "yes" | T.Untaint -> "no") time)))))))
end

module WEBI_Interp = MakeInterpreter(W_Input)(Client_interpreter.CL_Interp)(Device_interpreter_taint.DV_Interp)(Server_interpreter.SR_Interp)(Types_interpreter.T_Interp)

open W_Input
open WEBI_Interp
