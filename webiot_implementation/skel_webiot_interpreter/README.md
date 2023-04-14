# WEBIot implementation in SKEL
To compile
```
make normal
```

## Execution of the Programs

The implementation of Section 7 programs is in the following file.
```
utop webiTest.ml
```

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
There is the option to choose a non-deterministic semantics, without the scheduler.

For server and client code, we provide a parser to generate the CC, SS, and IC of an initial configuration.
Regarding devices, their evaluation function and the world oracle, we do not provide parsing.
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
