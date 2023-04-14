CMO = manads.cmo clientLanguage.cmo client_interpreter.cmo serverLanguage.cmo server_interpreter.cmo deviceLanguage.cmo device_interpreter.cmo webi.cmo webi_interpreter.cmo
GENERATED = lexer.ml parser.ml parser.mli
BIN=
FLAGS=

normal: run

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

#.mll.ml:
#	ocamllex $<

#.mly.ml:
#	menhir --infer -v $<
wtypes.ml:
	necroml wtypes.sk -o wtypes.ml
clientLanguage.ml:
	necroml wtypes.sk clientLanguage.sk -o clientLanguage.ml
serverLanguage.ml:
	necroml wtypes.sk serverLanguage.sk -o serverLanguage.ml
webi_init.ml:
	necroml wtypes.sk deviceLanguage.sk clientLanguage.sk serverLanguage.sk webi.sk webi_init.sk -o webi_init.ml
deviceLanguage.ml:
	necroml wtypes.sk deviceLanguage.sk -o deviceLanguage.ml
webi.ml:
	necroml -d webi.sk -o webi.ml
	ocamllex lexer.mll

server: wtypes.ml serverLanguage.ml
	ocamlc monads.ml wtypes.ml types_interpreter.ml serverLanguage.ml server_interpreter.ml

client: wtypes.ml clientLanguage.ml
	ocamlc monads.ml wtypes.ml types_interpreter.ml clientLanguage.ml client_interpreter.ml

device: wtypes.ml deviceLanguage.ml
	ocamlc monads.ml wtypes.ml types_interpreter.ml deviceLanguage.ml device_interpreter.ml

languages: server client device

run: monads.ml wtypes.ml clientLanguage.ml serverLanguage.ml deviceLanguage.ml webi.ml webi_init.ml
	ocamlc monads.ml wtypes.ml types_interpreter.ml \
		clientLanguage.ml client_interpreter.ml \
		serverLanguage.ml server_interpreter.ml \
		deviceLanguage.ml device_interpreter.ml \
		webi.ml webi_interpreter.ml \
		webi_init.ml webi_init_interpreter.ml
	menhir --infer -v parser.mly
	ocamlc parser.mli parser.ml lexer.ml

clean:
	rm -f *.cm[iox] *.o *~ $(GENERATED)
	rm -f wtypes.ml clientLanguage.ml serverLanguage.ml deviceLanguage.ml webi.ml webi_init.ml *.cm[io] *.out parser.automaton parser.conflicts parser.ml parser.mli lexer.ml
