NAME = ft_turing

# Order matters, no way to escape the interperter :()
SRCS = srcs/Types.ml srcs/Utils.ml srcs/Lexxer.ml srcs/Machinebuilder.ml srcs/Tape.ml srcs/main.ml
SRCS_OBJS_NATIVE = $(SRCS:.ml=.cmx)
SRCS_OBJS_INTERP = $(SRCS:.ml=.cmo)

INTERFACES = 
INTERFACES_OBJS = $(INTERFACES:.mli=.cmi)

PKGFLAGS = -package yojson,spectrum,core
LINKFLAGS = -linkpkg

all : 
	@echo "Usage: make native | interp"

native: setup $(INTERFACES_OBJS) $(SRCS_OBJS_NATIVE) 
	@echo native linking..
	ocamlfind ocamlopt $(PKGFLAGS) $(LINKFLAGS) -o $(NAME) $(SRCS_OBJS_NATIVE) -I srcs
	 $(MAKE) clean

interp: setup $(INTERFACES_OBJS) $(SRCS_OBJS_INTERP)
	@echo interp linking..
	ocamlfind ocamlc $(PKGFLAGS) $(LINKFLAGS) -o $(NAME) $(SRCS_OBJS_INTERP) -I srcs
	 $(MAKE) clean

setup :
	@echo Run eval opam env to load opam to env

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmx : 
	ocamlfind ocamlopt  $(PKGFLAGS) -c $< -I srcs

.ml.cmo : 
	ocamlfind ocamlc  $(PKGFLAGS) -c  $< -I srcs

.mli.cmi :
	ocamlc -c $< -I srcs

clean : 
	rm -f srcs/*.o srcs/*.cmx srcs/*.cmi srcs/*.cmo

fclean : clean
	rm -f $(NAME)

re : 
	@echo "Usage: make re_na | re_int"

re_int : fclean interp

re_na : fclean native