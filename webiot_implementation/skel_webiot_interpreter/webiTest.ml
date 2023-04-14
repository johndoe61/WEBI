#load "monads.cmo";;
#load "wtypes.cmo";;
#load "types_interpreter.cmo";;
#load "clientLanguage.cmo";;
#load "serverLanguage.cmo";;
#load "deviceLanguage.cmo";;
#load "client_interpreter.cmo";;
#load "server_interpreter.cmo";;
#load "device_interpreter.cmo";;
#load "webi.cmo";;
#load "webi_interpreter.cmo";;
#load "webi_init.cmo";;
#load "webi_init_interpreter.cmo";;
#load "parser.cmo";;
#load "lexer.cmo";;
module Lst = List

open Monads
open Wtypes
open Types_interpreter
open ClientLanguage
open ServerLanguage
open DeviceLanguage
open Client_interpreter
open Server_interpreter
open Device_interpreter
open Webi
open Webi_interpreter
open Webi_init
open Webi_init_interpreter
open Parser
open Lexer


(* Initialization *)
module T = Types_interpreter.T_Interp
module S = Server_interpreter.SR_Interp
module W = Webi_interpreter.WEBI_Interp
module WIT = Webi_init_interpreter.WEBI_Init_Interp

module SMap = Map.Make(String)


let parse prg =
  let lp = Lexing.from_string prg in
  Parser.webis Lexer.token lp

let rec split_tiers prg ls lc =
  (match prg with
  | W.Service p1 -> ([p1]@ls,lc)
  | W.Client p1 -> (ls,[p1]@lc)
  | W.WL (p1,p2) -> let (ls1',lc1') = split_tiers p1 ls lc in
                    let (ls2',lc2') = split_tiers p2 ls lc in
                    (ls1'@ls2',lc1'@lc2'))

let mk_WebService ls =
  let ws = WIT.M.extract (T.init_state ()) in
  let ws' = SMap.add_seq (Lst.to_seq ls) ws in
  ws'

let rec mk_hostMemories ls _HM_ =
  (match ls with
  | [] -> _HM_
  | h::t -> (match h with
            | (_,(_,hn)) ->  let _HM_' = SMap.add hn (WIT.M.extract (T.init_state ())) _HM_ in mk_hostMemories t _HM_'))



let ovenWindow =
            "service startAttacker :: hostA ( x ) :=
                     return ~(call(turnOnOven,$x, (z, return z)))

             client 1 call (startAttacker) := ()

             service turnOnOven :: hostB ( x ) :=
                     act(oven_turn_on, (oven,kitchen), \"\");
                     get(tmp,((oven,kitchen),\"\"));
                     return tmp

             client 2 call (startWindowManager) := 3

             service startWindowManager :: hostA ( x ) :=
                    i := 0;
                    while (i <= x) {
                      get(tmp,((thermometer,kitchen),\"\"));
                      if (tmp >= 25) then
                        act(window_open,(window,kitchen),\"\")
                      else
                        skip
                      fi;
                      i := i + 1
                    };
                    get(tmp,((oven,kitchen),\"\"));
                    return ~(return $tmp)"


let _ =
  let prg = parse ovenWindow in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_oven_kitchen ());
              WIT.M.extract (WIT.mk_thermometer_kitchen ());
              WIT.M.extract (WIT.mk_window_kitchen ())] in
  let conf = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_first_inits
    {W.wo = WIT.wo_oven_example; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_oven)
    {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 3}) in
  let oc = open_out "results/ovenWindow_scheduler.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc

let _ =
  let prg = parse ovenWindow in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_oven_kitchen ());
              WIT.M.extract (WIT.mk_thermometer_kitchen ());
              WIT.M.extract (WIT.mk_window_kitchen ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_oven_example; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_oven)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 3})) in
  let oc = open_out "results/ovenWindow_non_deterministic.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc

let secOvenWindow =
            "service startAttacker :: hostA ( x ) :=
                     return ~(call(turnOnOven,$x, (z, return z)))

             client 1 call (startAttacker) := ()

             service turnOnOven :: hostB ( x ) :=
                     act(oven_turn_on, (oven,kitchen), \"\");
                     get(tmp,((oven,kitchen),\"\"));
                     return tmp

             client 2 call (startWindowManager) := 10

             service startWindowManager :: hostA ( x ) :=
                    i := 0;
                    while (i <= x) {
                      get(tmp,((thermometer,kitchen),\"\"));
                      get(status,((oven,kitchen),\"\"));
                      if ((tmp >= 25) & (status = false)) then
                        act(window_open,(window,kitchen),\"\")
                      else
                        skip
                      fi;
                      i := i + 1
                    };
                    get(tmp,((oven,kitchen),\"\"));
                    return ~(return $tmp)"


let _ =
  let prg = parse secOvenWindow in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_oven_kitchen ());
              WIT.M.extract (WIT.mk_thermometer_kitchen ());
              WIT.M.extract (WIT.mk_window_kitchen ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_oven_example; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_oven)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 3})) in
  let oc = open_out "results/secOvenWindow.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc

(*to make it secure, this has to be done with shared variables.*)
let lightMotionSeizures =
  "service motionDetectedHandler :: hostA (x) :=
      i := 0;
      while (i <= x) {
        get(motion,((motion_sensor,house), \"\"));
        if (motion = 1) then
           act(setLevel_80,(switch,house),\"\")
        else
           skip
        fi;
        i := i + 1
      };
     return ~(return $motion)

  service motionStoppedHandler :: hostA (x) :=
     i := 0;
     while (i <= x) {
        get(motion,((motion_sensor,house), \"\"));
        if (motion = 0) then
           act(setLevel_0,(switch,house),\"\")
        else
           skip
        fi;
        i := i + 1
     };
     return ~(return $motion)

  service attack :: hostB (x) :=
    i := 0;
    while (i <= x) {
      get(v, ((switch,house),\"\"));
      if (v = 0) then
        act(setLevel_80, (switch,house),\"\")
      else
        act(setLevel_0, (switch,house),\"\")
      fi;
      i := i + 1
    };
    return v

  client 1 call (motionDetectedHandler) := 10
  client 2 call (motionStoppedHandler) := 20
  client 3 call (attack) := 10
   "


let _ =
  let prg = parse lightMotionSeizures in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_motion_sensor_house ());
              WIT.M.extract (WIT.mk_switch_house ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_seizures; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_seizures)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 3})) in
  let oc = open_out "results/lightMotionSeizures.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc


(*Similar to seizure attack*)
let lightMotionSignals = "
    service motionDetectedHandler :: hostA (x) :=
      i := 0;
      while (i <= x) {
        get(motion,((motion_sensor,house), \"\"));
        if (motion = 1) then
           act(setLevel_80,(switch,house),\"\")
        else
           skip
        fi;
        i := i + 1
      };
      return ~(call(attack,20,(y, return y)))

  service motionStoppedHandler :: hostA (x) :=
     i := 0;
     while (i <= x) {
        get(motion,((motion_sensor,house), \"\"));
        if (motion = 0) then
           act(setLevel_0,(switch,house),\"\")
        else
           skip
        fi;
        i := i + 1
     };
     return ~(return $motion)

   service attack :: hostB (x) :=
     i := 0;
     while (i <= x) {
        get(motion,((motion_sensor,house), \"\"));
        if (motion = 0) then
            act(setLevel_signalPattern, (switch,house),\"\")
        else
           skip
        fi;
        i := i + 1
     };
     return motion

  client 1 call (motionDetectedHandler) := 10
  client 2 call (motionStoppedHandler) := 20
"


let _ =
  let prg = parse lightMotionSignals in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_motion_sensor_house ());
              WIT.M.extract (WIT.mk_switch_house ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_seizures; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_seizures)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 3})) in
  let oc = open_out "results/lightMotionSignals.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc

let fireAlarm = "
    service smokeHandler :: hostA (x) :=
      i := 0;
      while (i <= x) {
        get(smoke,((smoke_sensor,house), \"\"));
        if (smoke = 1) then
           act(strobe,(alarm,house),\"\")
        else
           act(stop,(alarm,house),\"\")
        fi;
        i := i + 1
      };
      return ~(return $smoke)

  service strobeHandler :: hostA (phoneNumber) :=
     i := 0;
     while (i <= 10) {
        get(alarm_status,((alarm,house), \"\"));
        if (alarm_status = 1) then
           act(sendSMS,(phone,house),\"\") (- malicious number. For now act does not have parameters. to be changed -)
        else
           skip
        fi;
        i := i + 1
     };
     return ~(return $phoneNumber)

  client 1 call (smokeHandler) := 10
  client 2 call (strobeHandler) := 1234 (- malicious phone number -)
  client 3 call (strobeHandler) := 4321 (- good phone number -)
"


let _ =
  let prg = parse fireAlarm in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_smoke_sensor_house ());
              WIT.M.extract (WIT.mk_alarm_house ());
              WIT.M.extract (WIT.mk_phone_house ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_smoke; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_smoke)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 4})) in
  let oc = open_out "results/fireAlarm.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc


let secFireAlarm = "
    service smokeHandler :: hostA (x) :=
      i := 0;
      while (i <= x) {
        get(smoke,((smoke_sensor,house), \"\"));
        if (smoke = 1) then
           act(strobe,(alarm,house),\"\")
        else
           act(stop,(alarm,house),\"\")
        fi;
        i := i + 1
      };
      return ~(return $smoke)

  service strobeHandler :: hostA (phoneNumber) :=
     i := 0;
     while (i <= 10) {
        get(alarm_status,((alarm,house), \"\"));
        if ((alarm_status = 1) & (phoneNumber = 4321)) then
           act(sendSMS,(phone,house),\"\") (- malicious number. For now act does not have parameters. to be changed -)
        else
           skip
        fi;
        i := i + 1
     };
     return ~(return $phoneNumber)

  client 1 call (smokeHandler) := 10
  client 2 call (strobeHandler) := 1234 (- malicious phone number -)
  client 3 call (strobeHandler) := 4321 (- good phone number -)
"

let _ =
  let prg = parse secFireAlarm in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_smoke_sensor_house ());
              WIT.M.extract (WIT.mk_alarm_house ());
              WIT.M.extract (WIT.mk_phone_house ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_smoke; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_smoke)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 4})) in
  let oc = open_out "results/secFireAlarm.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc


(*The world oracle will produce a voice that is registered by alexa. A call to an alexa service will be produced.
 In case the service called is light on, nothing happens.
 In case the service called is light on please, the call is bad.
 All the devices are sensitive.
 This time is the service wich is flagged as taint.
 To make it secure one has to check that the suffix of the service is please in the alexa voice service.
 *)
let virtualAssistant = "
  service lightOn :: hostA (x) :=
    act(setLevel_80,(switch,house),\"\");
    return x

  service lightOnPlease :: hostB (x) :=
    act(window_open,(window,kitchen),\"\");
    return x

  service virtualAssistantVoice :: hostA (x) :=
    get(request,((virtual_assistant,living_room),\"\"));
    if (request = \"lightOn\") then
       return ~(call(lightOn,$x,(y, return y)))
    else
       return ~(call(lightOnPlease,$x,(y, return y)))
    fi

  client 1 call (virtualAssistantVoice) := ()
"

let _ =
  let prg = parse virtualAssistant in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_window_kitchen ());
              WIT.M.extract (WIT.mk_virtual_assistant_living_room ());
              WIT.M.extract (WIT.mk_switch_house ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_virtual_assistant; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_virtual_assistant)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 2})) in
  let oc = open_out "results/virtualAssistant.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc


(* I assume there is only one service without please, and one with the suffix.
   In case one have multiple services, with and without please, one just need to
   write a bigger If for each pleaseless service *)


let secVirtualAssistant = "
  service lightOn :: hostA (x) :=
    act(setLevel_80,(switch,house),\"\")

  service lightOnPlease :: hostB (x) :=
    act(window_open,(window,kitchen),\"\")

  service alexaVoice :: hostA (x) :=
    get(request,((virtual_assistant,living_room),\"\"));
       if (request = \"lightOnPlease\") then
          return ~(call(lightOn,$x,(y, return y)))
       else
          return ~(call(lightOn,$x,(y, return y)))
       fi

  client 1 call (alexaVoice) := ()
"


let _ =
  let prg = parse secVirtualAssistant in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_window_kitchen ());
              WIT.M.extract (WIT.mk_virtual_assistant_living_room ());
              WIT.M.extract (WIT.mk_switch_house ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_virtual_assistant; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_virtual_assistant)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 2})) in
  let oc = open_out "results/secVirtualAssistant.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc


let waterplant = "
 service addChemicals :: hostA (x) :=
   i := 0;
   while (i <= x) {
     act(add_sodium,(sodium_dispenser,waterplant),\"\");
     i := i + 1
   };
   return x

 service attack :: hostB (x) :=
   return ~(call(addChemicals,$x,(y, return y)))

 client 1 call (attack) := 10

 service alarmHandler :: hostA (x) :=
   i := 0;
   while (i <= x) {
     get(level,((sodium_sensor,waterplant),\"\"));
     if (level <= 6) then
       act(strobe,(alarm,waterplant),\"\")
     else
       skip
     fi;
     i := i + 1
   };
   return ~(return $x)

 client 2 call (alarmHandler) := 10

 service waterDistributionHandler :: hostA (x) :=
   i := 0;
   while (i <= x) {
     get(level,((sodium_sensor,valve),\"\"));
     if (level <= 4) then
       act(valve_open,(valve,waterplant),\"\");
       act(valve_close,(valve,waterplant),\"\")
     else
       skip
     fi;
     i := i + 1
   };
   return ~(return $x)

 client 3 call (waterDistributionHandler) := 10"


let _ =
  let prg = parse waterplant in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_valve_waterplant ());
              WIT.M.extract (WIT.mk_sodium_sensor_waterplant ());
              WIT.M.extract (WIT.mk_sodium_sensor_valve ());
              WIT.M.extract (WIT.mk_sodium_dispenser_waterplant ());
              WIT.M.extract (WIT.mk_alarm_waterplant ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_florida_waterplant; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_waterplant)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 4})) in
  let oc = open_out "results/waterplant.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc


let secWaterplant = "

 service addChemicals :: hostA (x) :=
   i := 0;
   while (i <= x) {
     get(level_valve,((sodium_sensor,valve),\"\"));
     get(level_waterplant,((sodium_sensor,waterplant),\"\"));
     if (level_valve <= 2) then
        if (level_waterplant <= 3) then
           act(add_sodium,(sodium_dispenser,waterplant),\"\")
        else
           skip
        fi
     else
        skip
     fi;
     i := i + 1
   };
   return x

 service attack :: hostB (x) :=
   return ~(call(addChemicals,$x,(y, return y)))

 client 1 call (attack) := 10

 service alarmHandler :: hostA (x) :=
   i := 0;
   while (i <= x) {
     get(level,((sodium_sensor,waterplant),\"\"));
     if (level <= 5) then
       act(strobe,(alarm,waterplant),\"\")
     else
       skip
     fi;
     i := i + 1
   };
   return ~(return $x)

 client 2 call (alarmHandler) := 10

 service waterDistributionHandler :: hostA (x) :=
   i := 0;
   while (i <= x) {
     get(level_valve,((sodium_sensor,valve),\"\"));
     get(level_waterplant,((sodium_sensor,waterplant),\"\"));
     if (level_valve <= 4) then
       if (level_waterplant <= 4) then
          act(valve_open,(valve,waterplant),\"\");
          act(valve_close,(valve,waterplant),\"\")
       else
          skip
       fi
     else
       skip
     fi;
     i := i + 1
   };
   return ~(return $x)

 client 3 call (waterDistributionHandler) := 10"


let _ =
  let prg = parse secWaterplant in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_valve_waterplant ());
              WIT.M.extract (WIT.mk_sodium_sensor_waterplant ());
              WIT.M.extract (WIT.mk_sodium_sensor_valve ());
              WIT.M.extract (WIT.mk_sodium_dispenser_waterplant ());
              WIT.M.extract (WIT.mk_alarm_waterplant ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_florida_waterplant; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_waterplant)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 4})) in
  let oc = open_out "results/secWaterplant.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc

(*Figure 2.2(c)(d) of paper: https://www.usenix.org/system/files/conference/atc18/atc18-celik.pdf*)
(*
This example shows two applications. The main one is the smokeHandler, which has its own semantics.
The purpose of the analysis is to show that, by adding another service, namely waterLeakageApp,
the semantics of the main service is broken.

Taint setting. We put as a source the client calling the waterLeakage app. What do we want to check is:
can the waterLeakageApp break the semantics of the smokeHandler. The answer is yes, and the taint tracker
will show that as soon as the waterLeakage app will close the valve, the functionality of the smoke alarm
break.
*)
let fireSprinkler = "
   service smoke_controller :: hostA (x) :=
    i := 0;
    while (i <= x) {
      get(smoke,((smoke_sensor,house),\"\"));
      if (smoke = 1) then
         act(strobe,(alarm,house),\"\");
         act(valve_open,(valve,house),\"\");
         (- avoided temperature info. it is an if -)
         act(sprinkler_open,(sprinkler,house),\"\")
      else
         skip
      fi;
      i := i + 1
    };
    return ~(return $x)

   client 1 call (smoke_controller) := 3

   service water_leakage_controller :: hostB (x) :=
    i := 0;
    while (i <= x) {
      get(moisture,((moisture_sensor,house),\"\"));
      if (moisture = true) then
         act(valve_close,(valve,house),\"\")
      else
         skip
      fi;
      i := i + 1
    };
    return ~(return $x)

    client 2 call (water_leakage_controller) := 3
"


let _ =
  let prg = parse fireSprinkler in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_smoke_sensor_house ());
              WIT.M.extract (WIT.mk_valve_house ());
              WIT.M.extract (WIT.mk_sprinkler_house ());
              WIT.M.extract (WIT.mk_moisture_sensor_house ());
              WIT.M.extract (WIT.mk_alarm_house ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_smoke_moisture; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_smoke_moisture)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 4})) in
  let oc = open_out "results/fireSprinkler.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc


let securityCamera = "
    service presenceHandler :: hostA (x) :=
      i := 0;
      while (i <= x) {
        get(motion,((motion_sensor,house),\"\"));
        if (motion = 0) then
           act(setLevel_80,(switch,house),\"\")
        else
           act(setLevel_0,(switch,house),\"\")
        fi;
        i := i + 1
      };
      return ~(return $x)

     service switchHandler :: hostB (x) :=
      i := 0;
      while (i <= x){
        get(switch_status,((switch,house),\"\"));
        if (switch_status = 80) then
           act(record_video,(camera,house),\"\");
           act(setLevel_0,(switch,house),\"\")
        else
           skip
        fi;
        i := i + 1
      };
      return ~(return $x)

     service cameraHandler :: hostA (x) :=
       i := 0;
       while (i <= 10) {
         get(status,((camera,house),\"\"));
         if (status = \"video\") then
            act(send_media,(camera,house),\"\")
         else
            skip
         fi;
         i := i + 1
       };
       return ~(return $x)

     client 1 call (presenceHandler) := 10
     client 2 call (switchHandler) := 10
     client 3 call (cameraHandler) := \"maliciousreceiver.com\"
     client 4 call (cameraHandler) := \"goodreceiver.com\"
"

let _ =
  let prg = parse securityCamera in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_motion_sensor_house ());
              WIT.M.extract (WIT.mk_switch_house ());
              WIT.M.extract (WIT.mk_camera_house ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_seizures; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_camera)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 4})) in
  let oc = open_out "results/securityCamera.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc



let secSecurityCamera = "
    service presenceHandler :: hostA (x) :=
      i := 0;
      while (i <= x) {
        get(motion,((motion_sensor,house),\"\"));
        if (motion = 0) then
           act(setLevel_80,(switch,house),\"\")
        else
           act(setLevel_0,(switch,house),\"\")
        fi;
        i := i + 1
      };
      return ~(return $x)

     client 1 call (presenceHandler) := 10

     service switchHandler :: hostB (x) :=
      i := 0;
      while (i <= x){
        get(switch_status,((switch,house),\"\"));
        if (switch_status = 80) then
           act(record_video,(camera,house),\"\");
           act(setLevel_0,(switch,house),\"\")
        else
           skip
        fi;
        i := i + 1
      };
      return ~(return $x)

     client 2 call (switchHandler) := 10

     service cameraHandler :: hostA (x) :=
       i := 0;
       while (i <= 10) {
         get(status,((camera,house),\"\"));
         if ((x = \"good\") & (status = \"video\")) then
            act(send_media,(camera,house),\"\")
         else
            skip
         fi;
         i := i + 1
       };
       return ~(return $x)

     client 3 call (cameraHandler) := \"evil\"

     client 4 call (cameraHandler) := \"good\"
"

let _ =
  let prg = parse secSecurityCamera in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_motion_sensor_house ());
              WIT.M.extract (WIT.mk_switch_house ());
              WIT.M.extract (WIT.mk_camera_house ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_seizures; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_camera)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 4})) in
  let oc = open_out "results/secSecurityCamera.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc


let secFireSprinkler = "
   service smokeHandler :: hostA (x) :=
    i := 0;
    while (i <= x) {
      get(smoke,((smoke_sensor,house),\"\"));
      if (smoke = 1) then
         act(strobe,(alarm,house),\"\");
         act(valve_open,(valve,house),\"\");
         (- avoided temperature info. it is an if -)
         act(sprinkler_open,(sprinkler,house),\"\")
      else
         skip
      fi;
      i := i + 1
    };
    return ~(return $x)

   client 1 call (smokeHandler) := 10

   service waterLeakageApp :: hostB (x) :=
    i := 0;
     while (i <= x) {
       get(status_smoke,((smoke_sensor,house),\"\"));
       get(moisture,((moisture_sensor,house),\"\"));
       if ((moisture = true) & (status_smoke = 0)) then
            act(valve_close,(valve,house),\"\")
       else
            skip
       fi;
       i := i + 1
    };
    return ~(return $x)

    client 2 call (waterLeakageApp) := 10
"


let _ =
  let prg = parse secFireSprinkler in
  let (ls,_I_) = split_tiers prg [] [] in
  let ws = mk_WebService ls in
  let _HM_ = mk_hostMemories ls (WIT.M.extract (T.init_state ())) in
  let _CC_ = {W._CC_e = T.lmk_empty;
              _CC_r = T.lmk_empty;
              _CC_c = T.lmk_empty;
              _CC_t = T.lmk_empty;
              _CC_boot = T.lmk_empty} in
  let _SS_ = {W._SS_e = T.lmk_empty;
              _SS_r = T.lmk_empty;
              _SS_a = T.lmk_empty;
              _SS_g = T.lmk_empty} in
  let _IC_ = [WIT.M.extract (WIT.mk_smoke_sensor_house ());
              WIT.M.extract (WIT.mk_valve_house ());
              WIT.M.extract (WIT.mk_sprinkler_house ());
              WIT.M.extract (WIT.mk_moisture_sensor_house ());
              WIT.M.extract (WIT.mk_alarm_house ())] in
  let (_,conf) = WIT.M.extract (WIT.M.extract (WIT.M.extract (WIT.M.extract (W.simulateWebi_scheduler
    {W.wo = WIT.wo_smoke_moisture; _E_= T.lmk_empty})
    ws)
    WIT.ic_eval_smoke_moisture)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = _I_;
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 4})) in
  let oc = open_out "results/secFireSprinkler.txt" in
  W.pp_debug oc (Lst.rev conf.debug);
  close_out oc

