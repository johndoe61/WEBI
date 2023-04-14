# WEBITaint implementation in SKEL
To compile
```
make taint
```

## Execution of the Programs

To run the example systems with the dynamic taint analyzer execute after having compiled:
```
utop webiTestTaint.ml
```

The implementation of Section 7 programs is in the following file.
`webiTestTaint.ml`

# Output Traces

The resulting traces of the systems are stored in the ```./results/```.

# Installing the Tool

## Skel

### Dependencies

* OCaml (4.12 or greater)
* necrolib (0.11.3)
* necro (0.2.10.1)
* menhir
* utop

#### Installation of Necrolib and NecroML via OPAM

You can install `necrolib` via `opam`. It is not in the official opam
repository yet, so it is necessary to first add the following repository.

```bash
opam switch create necro 4.12.0
eval $(opam env)
opam repository add necro https://gitlab.inria.fr/skeletons/opam-repository.git#necro
opam pin add necrolib 0.11.3
opam install necrolib
```
Then, you can install `necroml` from this repository.

```
eval $(opam env)
opam pin add necroml 0.2.10.1
opam install necroml
```
We remind that the tool will work only with this specific version of `necrolib` and `necroml`.
Further versions of the language differ in syntax.

# Description of the implementation

The language structure is defined in ```wtypes.sk``` file.
Server and client languages depends on ```wtypes.sk```.
The device language is an I/O interface providing ways to update or to query the memory of a device.
We instantiate every language semantics with a concrete instance of the interpreter.
There is the option to chose a non-deterministic semantics, without the scheduler.

For server and client code, we provide a parser to generate the CC, SS, and IC of an initial configuration.
Regarding devices, their evaluation function, the world oracle, the DWO, and the sinks, we do not provide parsing.
Indeed, the semantics of devices and of the world oracle has to be defined in Skel.
We show examples in the file ```webi_init.sk```.

## Device Declaration
A device must be defined in the semantics. One should define the semantics of the ACT, GET, and SENS operations of the device.
Moreover, to define and instance of the device, a user must use the constructor ```mk_device_instance``` and provide a geographical
location and the set of capabilities of the device.

## Instantiation of Devices Semantics
To provide to the interpreter the semantics of the devices that a program will consider at execution time, a user have to instantiate
the evaluation function of the devices. In the aforementioned files, we prefix the evaluation functions with ```ic_eval```. The semantics
of the devices is a partially applied function that the user passes to the interpreter.

## World Oracle
The world oracle is a parameter of the model; hence, a user can feel free to provide the best description of the world to execute programs.
In the examples, we prefix with ```wo_``` all the world oracle instances we define. Considering the formal nature of the work, all the
world oracles we define are discrete and simple. Nothing forbids to be more specific.

### DWO and Sinks
The DWO represents the tainting relations between devices. For instance, in the OvenSystem module we define a dependency relation between
the oven and the thermomether, as a running oven increases the temperature of a room; hence, the room's thermometer data are affected.
To define a DWO one has just to pass to the interpreter a list of dependencies of type (DeviceID * DeviceID) directly in the main file called
```webiTestTaint.ml```.

The sink function that we pass to the interpreter, is a lambda function we pass when calling the interpreter. To provide a declaration of
sensitive devices one has to write a partial function.

## Example of Instantiation for the Dynamic Taint Analyzer
The following is a legal program that supposedly we want to analyze.

```
let oven_program = "
service startAttacker :: hostA ( x ) :=
  return ~(call(turnOnOven,$x, (z, return z)))

bad client 1 call (startAttacker) := ()

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
```

To analyze, we provide 3 functions to build a webi configuration in the ocaml file ```webiTestTaint.ml```.
```
let rec split_tiers prg ls lc lm =
  (match prg with
  | W.Service p1 -> ([p1]@ls,lc,lm)
  | W.Client p1 -> (ls,[p1]@lc,lm)
  | W.Malicious p1 -> (ls,lc,[p1]@lm)
  | W.WL (p1,p2) -> let (ls1',lc1',lm1') = split_tiers p1 ls lc lm in
                    let (ls2',lc2',lm2') = split_tiers p2 ls lc lm in
                    (ls1'@ls2',lc1'@lc2',lm1'@lm2'))

let mk_WebService ls =
  let ws = WIT.M.extract (T.init_state ()) in
  let ws' = SMap.add_seq (Lst.to_seq ls) ws in
  ws'

let rec mk_hostMemories ls _HM_ =
  (match ls with
  | [] -> _HM_
  | h::t -> (match h with
            | (_,(_,hn,_)) ->  let _HM_' = SMap.add hn (WIT.M.extract (T.init_state ())) _HM_ in mk_hostMemories t _HM_'))

```

To intepret the program we do the following:
```
let _ =
  let prg = parse oven_program in
  let (ls,_Is_,_Ia_) = split_tiers prg [] [] [] in
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
    {W.wo = WIT.wo_oven_example; _E_= T.lmk_empty;
     sink = (fun s -> match s with
                    | ("window","kitchen") -> Sensitive
                    | _ ->  Insensitive);
     dwo = [(("oven","kitchen"),("thermometer","kitchen"))]})
    ws)
    WIT.ic_eval_oven)
    (W.IN,
     {W._HM_ = _HM_;
     _SS_ = _SS_ ;
     _CC_ = _CC_;
     _I_ = {_Ia_ = _Ia_; _Is_ = _Is_};
     _L_ = T.lmk_empty;
     _IC_ = _IC_;
     debug = T.lmk_empty;
     jcount = 3})) in
  let oc = open_out "results/oven_attack.txt" in
  W.pp_result oc conf;
  close_out oc

```

The first paramenter provided to the ```simulateWebi_scheduler``` is a record containing the description of the
world and its interactions and dependencies.
The field ```wo``` contains the world oracle.
The field ```sink``` contains the sink function.
The field ```dwo``` contains the dependencies between devices, in this case only ```(("oven","kitchen"),("thermometer","kitchen"))```

The second parameter we pass to the ```simulateWebi_scheduler``` is the semantics of the devices. This has been defined already in Skel
at this point.

The third parameter we pass to the ```simulateWebi_scheduler``` is a pair (scheduler_step,configuration).

The rest of the program prints the execution trace on a file.


### Extend Examples

If a user wants to define new examples, the user has to follow the textual tutorial provided in this ```README.md``` file.
In case the user wants to modify already existing examples, remember that one can easily change the logic of a program, but
to change device definition or the world oracle one, one has to modify the ```webi_init.sk``` or the ```webi_init_taint.sk```
file accordingly.
