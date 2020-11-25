main: parser.cmo lexer.cmo Ast.ml main.cmo
	ocamlc -o main lexer.cmo parser.cmo Ast.ml main.cmo
	$(info 7th)

parser.mli: Parser.mly
	ocamlyacc Parser.mly
	$(info 2nd)

parser.ml: parser.mli

lexer.ml:  Lexer.mll
	ocamllex Lexer.mll
	$(info 1st)

lexer.cmo: lexer.ml parser.cmi
	ocamlc -c lexer.ml
	$(info 4th)

parser.cmi: parser.mli
	ocamlc -c parser.mli
	$(info 3rd)

parser.cmo: parser.ml lexer.ml parser.cmi
	ocamlc -c parser.ml
	$(info 5th)


main.cmo: main.ml
	ocamlc -c main.ml
	$(info 6th)


clean:
	rm *.cmo *r.ml *.mli *.cmi main

