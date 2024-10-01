# Description of the Repository

In this repository we provied additional material for the submission at IEEE CSF 25'.
The structure of the repository is the following:
* WEBIot implementation (`./webiot_implementation`): section 5
  - WEBIot scheduler semantics and proofs (`./webiot_scheduler_semantics_proof`).
  - SKEL WEBIot interpreter (`./skel_webiot_interpreter`)
* WEBITaint implementation (`./webitaint_implementation`): section 6
  - WEBITaint semantics and proof (`./webitaint_semantics_proof`).
  - SKEL WEBITaint analyzer (`./skel_webitaint_analyzer`)
* Section 7 Examples (`./section_7_examples`)

# Virtual Machine

We provide a virtual machine with the tools already installed.
The link is the following: [HERE](https://drive.google.com/file/d/1IG1xItuiFfhsC5j741vaOMAdHUneqGXv/view?usp=share_link)

To log in in the system use the following credentials.
```
username : artifact
password : artifact
```

The artifact can be found in the following directory:
```
cd ~/Desktop/Artifact/WEBIoT
```

To execute the programs, follow the `README.md` in the following folders.
* For the WEBIoT interpreter:
`webiot_implementation/skel_webiot_interpreter`

* For the WEBITaint interpreter:
`webitaint_implementation/skel_webitaint_interpreter`
