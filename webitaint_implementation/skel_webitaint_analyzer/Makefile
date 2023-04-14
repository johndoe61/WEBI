CMO = manads.cmo clientLanguage.cmo client_interpreter.cmo serverLanguage.cmo server_interpreter.cmo deviceLanguageTaint.cmo device_interpreter_taint.cmo webi_taint.cmo webi_interpreter_taint.cmo
GENERATED = lexer_taint.ml parser_taint.ml parser_taint.mli
BIN=
FLAGS=

taint: run_taint

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

wtypes.ml:
	necroml wtypes.sk -o wtypes.ml
clientLanguage.ml:
	necroml wtypes.sk clientLanguage.sk -o clientLanguage.ml
serverLanguage.ml:
	necroml wtypes.sk serverLanguage.sk -o serverLanguage.ml
webi_init_taint.ml:
	necroml -d webi_init_taint.sk -o webi_init_taint.ml
deviceLanguageTaint.ml:
	necroml wtypes.sk deviceLanguageTaint.sk -o deviceLanguageTaint.ml
webi_taint.ml:
	necroml -d webi_taint.sk -o webi_taint.ml
	ocamllex lexer_taint.mll
run_taint: monads.ml wtypes.ml  clientLanguage.ml serverLanguage.ml deviceLanguageTaint.ml webi_taint.ml webi_init_taint.ml
	ocamlc monads.ml wtypes.ml types_interpreter.ml \
		clientLanguage.ml client_interpreter.ml \
		serverLanguage.ml server_interpreter.ml \
		deviceLanguageTaint.ml device_interpreter_taint.ml \
		webi_taint.ml webi_interpreter_taint.ml \
		webi_init_taint.ml webi_init_interpreter_taint.ml
	menhir --infer -v parser_taint.mly
	ocamlc parser_taint.mli parser_taint.ml lexer_taint.ml
clean:
	rm -f *.cm[iox] *.o *~ $(GENERATED)
	rm -f wtypes.ml clientLanguage.ml serverLanguage.ml deviceLanguageTaint.ml webi_taint.ml webi_init_taint.ml *.cm[io] *.out parser_taint.automaton parser_taint.conflicts parser_taint.ml parser_taint.mli lexer_taint.ml
