# /dev/log for ft_turing
The goal of this project is to create a turing machine in OCaml, a functional programming language as well as write a few simple programs that can be run on said turing machine.

# Week 1
## Ocaml reading
To get things started, we need to install **Opam** which is the package manager for OCaml
```
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
sudo apt-get install bubblewrap
opam init
eval $(opam env --switch=default)
ocaml --version
```

I studied this [book](https://www.ps.uni-saarland.de/~smolka/drafts/prog2021.pdf) for basic functional programming paradigm and the official [docs](https://ocaml.org/docs/installing-ocaml) for workflow and technical details.

The file terminology is closely similar to C; `.mli` files are sources for interfaces, like `.h`, it will produce a `.cmi` object file on compilation. `.ml` files are sources for implementations, like `.c`, it will produce a `.cmx` object file on compilation. File with other extensions are considered native C files. 

We will be using manual compilation for this project, so build tools like dune are forbidden. For manual compilations using ocamlc or ocamlopt, the **order of the source file and object files matter when compiling**. Which means if codeA is dependent on codeB, codeB needs to be compiled first. The general compilation commands are like so
```
ocaml -c <interfaces> # mli -> cmi
ocamlopt -c <sources_in_order> -I <interfaces_dir> # ml -> cmx
ocamlopt -o <exename> <cmx_dir> -I <interfaces_dir> # produces a.out
```

If we wanted to use **external** libraries installed by opam, we nee to surround our compalation commands with **ocamlfind** and with different flags for compiling and linking.
```
ocamlfind <compile command> -thread -package <external installed packages> # compilation
ocamlfind <link command> -thread -package <external installed packages> -linkpkg # linking
```

## Workflow setup
I have structured my project in the following structure; All sources and modules will be inside the `srcs` directory
```
.
├── Makefile
├── README.md
└── srcs
    ├── main.ml
    ├── module.ml
    └── module.mli
```

The makefile will be something simple like this to get things going 
```
NAME = ft_turing

SRCS_LEVEL0 = srcs/module.ml
SRCS_LEVEL_TOP = srcs/main.ml
SRCS = srcs/module.ml srcs/main.ml
SRCS_OBJS_NATIVE = $(SRCS:.ml=.cmx)
SRCS_OBJS_INTERP = $(SRCS:.ml=.cmo)

INTERFACES = srcs/module.mli
INTERFACES_OBJS = $(INTERFACES:.mli=.cmi)

all : 
	@echo "Usage: make native | interp"

native: $(INTERFACES_OBJS) $(SRCS_OBJS_NATIVE) 
	@echo native linking..
	ocamlopt -o $(NAME) $(SRCS_OBJS_NATIVE) -I srcs

interp: $(INTERFACES_OBJS) $(SRCS_OBJS_INTERP)
	@echo interp linking..
	ocamlc -o $(NAME) $(SRCS_OBJS_INTERP) -I srcs

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmx : 
	ocamlopt -c $< -I srcs

.ml.cmo : 
	ocamlc -c  $< -I srcs

.mli.cmi :
	ocamlc -c $< -I srcs

clean : 
	rm -f srcs/*.o srcs/*.cmx srcs/*.cmi srcs/*.cmo

fclean : clean
	rm -f $(NAME)
```

The .SUFFIXES rule is to let Makefile recognise our Ocaml file suffixes, which is not in the known suffixes by default `.c, cpp, .o ...`.

## Finite state machines 
The **most basic unit of computation**. A machine or tool with limited memory and power which can be used to compute.

A FSM can be represented via a graph.
![image](https://hackmd.io/_uploads/SJnujZ9_p.png)

States (nodes) - the mode the machine is in
Transitions (edges) - the action of switching between states using symbols

`a` is a starting state
`d` is an ending/accepting state
`{0, 1}` are symbols to define transitions

The FSM describe above can be used to do 2 things:

### Generate strings
1. Start at starting state
2. Take instructions as random
3. Finish only on accepting state
4. The resultant string is the transitions that we made. "101110" for example.

### Accept/Decline strings
1. Start at starting state
2. Start at first symbol from string input
3. Follow transitions bsaed on the input until the end of input
4. If the state lands on an accept state, the string is accepted.

A formal definition of a FSM would be using a 5-tuple
![image](https://hackmd.io/_uploads/r1ox-fqOT.png)

In the particular example above, the user inputs can be considered as languages.

![image](https://hackmd.io/_uploads/S1R0zMcdp.png)
Above is an example of a FSM that accepts strings that has 0011 in it.

![image](https://hackmd.io/_uploads/HJtv7M5u6.png)
Above is an example of a FSM that accepts string which accepts strings that dont have 0011 in it.

To represent the **machines language** in formal definition, it can be done like so 
$$
L(M_1)
$$

since the languages are sets, we can say that
$$
L(M_1) = \overline{L(M_2)}
$$

Consider the FSM below : 
![image](https://hackmd.io/_uploads/SymLdz5Op.png)

As we can see, the strings which are to be accepted are `10, 01, 001, 0001, 00001 .....`. However, it does not handle behaviour like `100` explicitly.

In the machine above, there are dead states (undefined transitions). Those are declared implicitly, and once a machine goes into a dead state, it stays in a dead state.

![image](https://hackmd.io/_uploads/ByyiRz9O6.png)

With that said, the **formal definition** of computation is 

$$
M = (Q, \sum, \delta, q_0, F)
$$

Let `w = w1w2w3w4....wn` be a string where `wi` is a part of symbols. And `M` accepts `w` is there is a sequence of states

$$
r_0,r_1,r_2,r_3....r_n\ in\ Q
$$

$$
r_0 = q_0\ and\ \delta(r_n, w_{i + 1})=r_{i+1}\ for\ 0 \leq i \leq n
$$

$$
r_{n} \in F
$$

As known as, M recognizes language A if `A = {w | M accepts W}` 

# Week 2

## Regular languages
Since finite state machines have **limited memory**, it is impossible for it to process languages which needs memory to be accepted / rejected. such impossible languages are languages which need to have matching parenthesis, languages which needs to match number of characters and so on. Regular languages are languages which can be represented **by at least 1** Finite State Machine.

Such languages are also closed in operations with operations like **union, concatnation and star**. This means the result of thos operations (union, concatnation and star) on regular language will be a regular language also.

```
A = {aa, b} B = {x, yy}

A ∪ B = {aa, b, x, yy}
A ∘ B = {aax, bx, aayy, byy}
A* = {aab, aaaa, baa, aabbaab .....}
```

This is proven by the ability to construct a FSM with the combinations of the 2 FSM where the new FSM is able to recognize the language of the result of the operation.

```
For A ∪ B,

Q = {x, y, z}
Q2 = {3, 4, 5, 6, 7}

M1 = (Q, {a, b}, δ1, q1, F1)
M2 = (Q2, {a, b}, δ2, q2, F2)

δ3((r1, r2), a) = (δ1(r1, a), δ2(r2, a))
q3 = (q1, q2)
F3 = {(r1, r2) | r1 in F1 pr r2 in F2}
M3 = (Q1 x Q2, δ3, q3, F3)
```

Such a machine is possible and valid, hence the proof is complete.

## Regular expressions
A regular expression is something used to express / represent valid strings in regular languages. For example, if we have a symbol set `Σ = {'a', 'b', 'c', 'd'}`, the regex `a(b|c)d` will generate the strings `{abd, acd}`. There are a few general nonataions with set definitions;

```
| - Union, or
{} - Star, closure
a+ - one or more
[a], a? - optional
```

## Pumping lemma for regular languages
The pumping lemme is a way to determine if a language **is a regular language or not**. The key idea is that if a FSM has a cycle, you can go around the cycle once or multiple times and the strings generated / inputted should be also in the same language (accepted)

![image](https://hackmd.io/_uploads/BJEyhl4Yp.png)

All strings are in the form `S = x(iy)z` where `i > 0`.

If `A` is a regular language and `|s| >= P (Pumping length, strings that are longer than this will have a cycle)`. The string can be devided into `s = xyz` Such that `y` is repeatable and the resulting string is still in the language. 

On a non-regular language like `B={0^n1^n | n >= 0}` and suppose the pumping length is 7, a valid string in this language would be `00000001111111`. If the language is regular, we can split the string above into 3 parts. Suppose the `y` part is `000111` in the middle. If we duplicate the `y` part once, we will result in `00000001110001111111`, which is not accepted by the language, which contradicts the regular language rule. 

The above example also will have similar results if we move y somewhere else, `0000000`**`000`**`1111111` and `0000000`**`111`**`1111111` are all invalid strings of the language.

## Context free grammars and languages
A **formal grammar** describes how to form strings from an alphabet (symbol) from a language. A Context Free Grammar (CFG) is a formal grammar whose production rules can be applied to a non-terminal symbol regard;ess of context. They can generate Context Free Languages (CFL)

For example, for the CFG `E -> T + Fi`
- Non-terminals are the variables (E, T, F)
- Terminals are the symbols/operators (i, +)
- Rules are productions. There can be multiple rules in 1 CFG (E -> T + Fi)
- The start variable is E

Given the CFG below,
```
E -> E + T or T
T -> T x F or F
F -> (E) or a
```
To derive the language starting from the symbol E; 
```
E => E+T => T + T => F + T => a + T => a + F
    => a + (E) => a + (T) ..... => a + (a x a)
```
We pick the leftmost variable to expand first, the effect will be the same if we picked the right most variable. The derivation can also be written as 
`E =>* a + (a x a)`.

Just like Regular languages, there is a pumping lemma for CFLs. Instead of a loop in the FSM, the loop will be occuring in the non terminal at the parse tree.

## Pushdown Automata
A **computing model** like FSM just that it also interacts with stack memory. It is by design non-deterministic since transitions may also depend on the stack top and may push to the stack. It recognizes CFLs.

FSM transition
![image](https://hackmd.io/_uploads/rJ57BWVFp.png)

PDA transition
![image](https://hackmd.io/_uploads/BJ3iB-4Y6.png)

*It is not possible to pop an emtpy stack*

Formal definition
![image](https://hackmd.io/_uploads/BJaTIW4tp.png)


## Turing machines
A model of computation like FSA and PDA, however it is more powerful and is a model for modern computers (turing complete). It recognizes [Decidable languages or Turing recognizable languages](https://math.stackexchange.com/questions/25802/recognizable-vs-decidable).

The input for a TM comes in the form of a tape with symbols on it like so
![image](https://hackmd.io/_uploads/BJSIMzVKa.png)

The underlying FSM operates like so
![image](https://hackmd.io/_uploads/r1xNffEt6.png)
1. Reads current symbol
2. Update current symbol in cell
3. Move one cell left / right. 

# Week 2
## Logging and Lexxer development
In my OCaml project, I have installed some external dependencies, `yojson` for JSON read / write and `spectrum` for terminal color and fomratting.

The way yojson works by handling JSON data structures is like so.
```
type t = [ 
| `Null
| `Bool of bool
| `Int of int
| `Float of float
| `String of string
| `Assoc of (string * t) list
| `List of t list
 ]
 ```
 
For example, our `unary_sub` machine is typed by yojson like so : 
```
{
    "name": String,
	"alphabet": List [String],
	"blank": String,
	"states": List [String],
	"initial": String,
	"finals": List [String]
	"transitions": List [
        Assoc("scanright", t),
        Assoc("scanleft", t),
        ...
    ]
}
```

The logic to tokenize the machine definition is consutrcted like so

1. Check argv
2. Read file of machine definition
3. Extract items of the json object using Yojson.Basic.from_file
4. Check for required fields, dupes and unknowns in the top level of json object
5. Extract all individual items from validated items (and perform validation)
6. Construct the machine struct using extracted items

The mentioned struct will look something like this, stored as an OCaml record. Note that this struct is NOT the machine definition, just the configuration of the machine.

```
 {
  name = name;
  alphabet = symbols;
  blank = blank;
  stateslst = stateslst;
  states = states;
  initial = initial;
  finals = finals;
  transitions = transitions
}
```

## Machine parsing
The definition of a Turing machine is like so [According to wiki](https://en.wikipedia.org/wiki/Turing_machine)
![image](https://hackmd.io/_uploads/BJvccuI5T.png)

- ![image](https://hackmd.io/_uploads/HyCGodU9T.png) is a finite, non-empty set of tape symbols
- ![image](https://hackmd.io/_uploads/rJrrsOLqa.png) is a blank symbol
- ![image](https://hackmd.io/_uploads/BkoUiuUcp.png) is the set of input symbols ; The symbols which are allowed to appear in the initial tape content.
- ![image](https://hackmd.io/_uploads/HkCb3OLqp.png) is a finite; non - empty set of states
- ![image](https://hackmd.io/_uploads/r1yH2dU9T.png) is the initial state
- ![image](https://hackmd.io/_uploads/rJi8hu85a.png) are the final / accepting states. The initial tape contents is said to be accepted by M if it eventually halts in a state from F
- ![image](https://hackmd.io/_uploads/SJCa2uLcp.png) are the transition state functions

Since we have a configuration defined, we have to create a machine with the defnition above based on the defined configuration. The resulting record shape to transform is to 

```
type machine_definition = {
	name : string;
	alphabet : CharSet.t; (* Can also be input, since subject dont have a seperate definition for input chars only *)
	blank : char;
	transition_table : state_transitions array; (* Also contains all states and final states / multitype *)
	current_state : state_index; (* Will initialize with initial state *)
}

```
As we can see, most of the data can be just moved from the configuration object, however for the transition table, we need to do further processing to be able to parse that. for the transitions that are defined like so 
```
"transitions": {
		"scanright": [
			{
				"read": ".",
				"to_state": "scanright",
				"write": ".",
				"action": "RIGHT"
			},
			{
				"read": "1",
				"to_state": "scanright",
				"write": "1",
				"action": "RIGHT"
			},
			{
				"read": "-",
				"to_state": "scanright",
				"write": "-",
				"action": "RIGHT"
			},
			{
				"read": "=",
				"to_state": "eraseone",
				"write": ".",
				"action": "LEFT"
			}
		],
		"eraseone": [
			{
				"read": "1",
				"to_state": "subone",
				"write": "=",
				"action": "LEFT"
			},
			{
				"read": "-",
				"to_state": "HALT",
				"write": ".",
				"action": "LEFT"
			}
		],
```
We can execute the following logic to construct a transition table based on the input. first of all, we can define a type that represents a transition table row / (scan_write, erase_one) and then map the rows with values that define that transition (read, to_state, write and action).

We can define each individual transisiton value and each state transition (contains multiple transition values) like so

```
type transition = Defined of read_char * write_char * action * to_state | Undefined (*Two possible values, can be Defined aka (char * action * state_idx) or Undefined aka unit*)
type state_transitions = Normal of (string * transition array) | Final of string (*Two possible values, can be Normal aka (string * transition array) or Final aka String*)
```
With the types above, we can construct a transition table by doing the following

1. Initialize an empty transitions array, with no rows, with size of the number of transition table rows, to all final empty states
2. Iterate through all of the states in the configuration, calls a function that creates a transition table entry for each element being iterated.

The function that creates a transition table entry for each element will 
1. Check if the state is in finals or not. If it is int finals, create a final type transition with the state name and set it at the transition table created just now with the index from the statelist iteration
2. Find the transition entry in the config json using the state name. Once found, seriaize that json to beocme a list of associates. Create an empty array to store all individual transitions
3. Iterate the associates and extract invididual values. Validate them and put them in the array from previous step

As we can see now, we are able to parse the machine and display its information
![image](https://hackmd.io/_uploads/SyPnM9jcT.png)


# Week 3
## Tape
Now we have parsed the machine as the input, the second input left to process is the tape input. This one is simple, we can represent the entire tape structure like so

```
{
    blank: char; # What a blank character should be
    data: string; # The current characters on the tape
    head: int; # the index of the head
    size: int; # the size of the tape (to write blank characters when moving out of bounds)
 }
```

We will also have a `Tape.create` function that accepts / parses the input user argument, validates that the symbols are in the machines alphabet and create an instance of a srtucture above. We will also have functions to

- Read character at current head of a tape
- Write character at current head of a tape
- Convert a tape into a user friendly string
- Move a tape left / right and adds a blank character is the size is out of bounds (returns a new tape)

Once those functions are implemented, we can see the representation of the same tape, moved in different directions.
![image](https://hackmd.io/_uploads/H1qxQ5i9a.png)


## Executor
This part is where the turing machine starts executing the commands entered by the user on the machine defined.

This helpful [resource](https://literateprograms.org/turing_machine_simulator__ocaml_.html) mentioned how a turing machine should evaluate the code in terms of programming. Notice the simulator section of the image below, it is quite similar to what we are tring to do now.

![Screenshot 2024-02-04 at 14.13.08](https://hackmd.io/_uploads/rJRl2sn96.png)

The first thing to do is to trace the current state, which we need to obtain from the machine definitions using the current state index. For debugging purposes, I also print out the current state *of the tape* to stdout 
```ocaml
(* Print the current tape *)
let tape_str = Tape.tape_to_str tape in
Spectrum.Simple.printf "@{<grey>%s@}\n" tape_str;

(* Get current state *)
(* returns  state_transitions array *)
let curr_state = machine.transition_table.(machine.current_state) in 
```
After we get the current state (Final or Normal in our case, accpeting states and rejected states are ambigious), We will halt execution and return if the state is a Final state. If its not a Final state however, we 
1. Reads the current character in the tape head
2. Based on the current states transition table, find a transition with a domain which matches the character
3. If no such transition exists, halt execution and return an arbitrarity state string
4. If such transition exists, update tape (write_char and move) and machine state (curr_state toState) 
5. Call simulate again with a new tape and another state of machine definition.

The above is implemented and can be tested with a sample input given; `111-11=` with the machine definition generated above as well.
![image](https://hackmd.io/_uploads/HJK3WqocT.png)

# Week 4
## Unary subtraction program.
Every turing machine can be represented by a finite state machine; for the unary subtraction program that was given, it can be represented as such

![image](https://hackmd.io/_uploads/BkeCSCaca.png)

The program follows a simple algorithim
1. Travel alll the way to the right until a '=' symbol is found and replace it with a blank character
2. Change the state to __eraseone__, which will start scanning from right to left instead until it encounters another '1'. Replace the '1' with a '=' to mark the new end of the equation. If a '-' is encountered however instead of a '1', which means there are nothing to subtract anymore; and the program halts
3. Change the state to __subone__ which traverses all the '1's from right to left until a '-' is found, change state to __skip__ (keep moving left)
4. Keep moving until a '1' is found. Change the '1' to a blank and revert to initial state and move right.

![image](https://hackmd.io/_uploads/SJwJQJkpT.png)


## Unary addition program
The program should behave like a base-1 calculator, an example of input output is like so `11+11 = 1111` . The unary addition program can be represend visually in a graph like so

![image](https://hackmd.io/_uploads/Sy2NiCWo6.png)

The program does the following: 
1. Travel all the way to the right until I see a '=', reaplce it with blank
2. Change state to __addone__ which now traverses from right to left until it encounters another '1'. Turn that '1' into '=' to mark the new right end of the tape; If a '+' is found instead, halt the program (not needed?)
3. Change state to __replace_add__ which traverses until a '+' is found. Replace that '+' with '1' and change to __end__ state, which goes right until '=' is found; change it to '.' and halt the program. 

![image](https://hackmd.io/_uploads/ryJpvaAhp.png)


## Palindrome prediction program
This program will accept inputs which are palindromes, and it will write the result `y` or `n` at the end of the tape before halting.
![image](https://hackmd.io/_uploads/HJadkZfjT.png)

1. Read the current character. If its '0', replace with '-' and change state to __get_last_zero__ (the same thing for '1'). If its '-' (checked character) , keep moving right until a '0' or '1' is found.
2. Move right until we hit a blank or 'y' (signifies end of tape), write 'y' as a place holder and change state to __is_zero__ and read from right to left.
3. Move right until we hit a blank, '0' or '1'
    - If '1' is read, change state to __deny__, which moves from left to right, until we reach 'y'/'n', write 'n' and halt
    - If a blank is read, halt since we already finished checking
    - If a '0' is read, replace with '-' and change state to __get_most_left__. (Replace the variables above for the '1' case)
4. Traverses from right to left until a blank is found, change state to __init__.

![image](https://hackmd.io/_uploads/SkBWmyJpa.png)

## Pair prediction program
This program will determine if the input has equal numbers of `0`s and `1`s and they are next to each other.
![image](https://hackmd.io/_uploads/HyTg6Z0jp.png)

1. Read the current character until '0' or '1'. If its '0', replace with '-' & change state to __get_last_one__. If its '1' however, chagne state to __deny__ moves right until a blank, 'y' or 'n' is found, write 'n' & halt. 
2. Moves right until we hit a blank or a 'y' (signifies end of tape), write a 'y' as placeholder and change state to __is_one__ and read from right to left.
3. Read until a blank, '0' or '1' :
    - If '1' is read, replace it with '-' and change state to __get_most_left__.
    - If '0' is read, change state to __deny__ and move right
    - If blank is read, halt since we already finished checking.
4. Traverse left until a blank is found, change state to __init__.

![image](https://hackmd.io/_uploads/BymQmkyTp.png)


## Even length program
This program will determine if the length of the input is even
![image](https://hackmd.io/_uploads/SJ76xf0iT.png)

1. Read current character, if its '0', change state to __get_pair_zero__. If its blank, write 'y' & halt
2. Read current character. If its a '0', change state to __init__. If its bkank, write 'n' and halt.

![image](https://hackmd.io/_uploads/Hy4V7y1TT.png)


# Week 5
## Universal turing machine
A universal turing machine is a turing machine that can **simulate** other turing machines. Our previous turing machine programs are run using the turing machine we wrote in OCaml, the challenge of this is to write a turing machine in turing machine language, replacing the turing machine we wrote using OCaml.

The resources are scarce for this one, I can hardly find any good ones with a low skill floor without any hard prerequisites. I have looked up on [Minskys UTM](https://www.researchgate.net/publication/263805945_Minsky's_small_universal_Turing_machine) and I find it require more background knowledge than what we already have up to this point.

I kept searching and stumbled upon [This awesome open source repo by jileventreur](https://github.com/jileventreur/ft_turing) which happen to have a minimal implementation of a simple turing machine. My apporach will be heavily inspired by his credible work. However the documentation was not sufficent and verbose enough to make me understand without further analysis; hence the motivation to write this document.


## Definitions of 'running' and 'simulating'
When we 'run' a program in the context of state machines (a set of instructions which changes **some** state), What we are doing is that we are :
- Reading the instruction
- Parsing / decoding / give meaning to the instruction that we read
- Apply changes to **some** state based on the instruction that we parsed
- Read the next (this "next" action could also be a state) and repeat indefinitely

This loop is silimiar to the [Fetch-Decode-Execute cycle](https://en.wikipedia.org/wiki/Instruction_cycle) in modern CPUs. **Fetch** being reading an instruction, **Decode** being parsing the instruction and **Execute** being applying the state changes stated in the parsed instructions 

![image](https://hackmd.io/_uploads/H1_9JpAna.png)

To 'simulate' this behaviour, we need to write a program (set of state-changing instructions, in this case is our UTM) that does the fetch-decode-execute cycle we see earlier and run the new program on a machine (in this case is our OCaml written turing machine)

We kept mentioning that the execution part of the cycle changes **some** state; the 'state' here in modern CPUs could be registers, files in your file system, memory contents etc. As in the context of UTM, the 'state' here can be the state of the tape head and the tape.

## Encoding / compilation
Before we plan out the integration of the simulation, it is crucial to know what will the input to our so called `simulation` be.

A true UTM will have its own complex set of rules and encoding methods which can run ANY turing machine in existence. However we wont do that here, we may assume that **we know what machine we are trying to simulate prior to building the UTM**.

With that said, the encoding that we will be using to encode/compile a turing machine and its input to feed into a built UTM can be more laxed like so.

`1.+=|.|ABCDE|A|E|A.A.RA1A1RA+A+RA=B.LB1C=LB+E.LC1C1LC+D1RD1D1RD=E.R|_11+1111=`

Notice how the information is sperated by pipes.
The first section denotes the **alphabet** of the machine

The second section denotes the **blank character** the machine uses

The third section denotes the **states** the machine has, represented by a single character

The foruth and fifth section denotes the **initial and final states** respectively

The sixth section denotes the **transitions** of the machine; with each transition being seperated into 5 characters like so `A.A.R A1A1R A+A+R A=B.L B1C=L B+E.L C1C1L C+D1R D1D1R D=E.R`. Each subsection here has 5 characters, and they represent _state_, _read character_, _to_state_, _write character_ and _action_ respectively

The final section will be the **input** of the input machine. Notice that we have an underscore `_` at the beginning, which denotes our initial head position.

Here is the original, uncompiled turing machine
```json
{
	"name": "unary_add",
	"alphabet": [
		"1",
		".",
		"+",
		"="
	],
	"blank": ".",
	"states": [
		"A",
		"B",
		"C",
		"D",
		"E"
	],
	"initial": "A",
	"finals": [
		"E"
	],
	"transitions": {
		"A": [
			{
				"read": ".",
				"to_state": "A",
				"write": ".",
				"action": "RIGHT"
			},
			{
				"read": "1",
				"to_state": "A",
				"write": "1",
				"action": "RIGHT"
			},
			{
				"read": "+",
				"to_state": "A",
				"write": "+",
				"action": "RIGHT"
			},
			{
				"read": "=",
				"to_state": "B",
				"write": ".",
				"action": "LEFT"
			}
		],
		"B": [
			{
				"read": "1",
				"to_state": "C",
				"write": "=",
				"action": "LEFT"
			},
			{
				"read": "+",
				"to_state": "E",
				"write": ".",
				"action": "LEFT"
			}
		],
		"C": [
			{
				"read": "1",
				"to_state": "C",
				"write": "1",
				"action": "LEFT"
			},
			{
				"read": "+",
				"to_state": "D",
				"write": "1",
				"action": "RIGHT"
			}
		],
		"D" : [
			{
				"read": "1",
				"to_state": "D",
				"write": "1",
				"action": "RIGHT"
			},
			{
				"read": "=",
				"to_state": "E",
				"write": ".",
				"action": "RIGHT"
			}
		]
	}
}
```

> Note: the main limitation of this encoding is that we cant have more than 52 states, which we are representing using a single character

## State machine and Tape hacks
These are some methods which will be commonly used to manipulate the tape that will help us develop more high-level instructions when we build our UTM

### Arbitrary move
If you want to move an arbitrary number of steps in a direction, the naive way to do so is to find a way to put a character at that spot, then move at that direction indefnitely until that character is found. There is a way to do this without the hassle of placing down a marker by using the current tape head state as a counter instead like so:

![image](https://hackmd.io/_uploads/Hk28KaAnT.png)

This approach is somewhat similar of creating a linked list where the to_state points to the next node for all characters read. The write character will remain unchanged from the read character.

### Arbitrary write
This method is derived from the Arbitrary move, where you want to write a character after moving a certain amount of steps. A small modification to the Arbitrary move states can be done to acheive this

![image](https://hackmd.io/_uploads/S1XJoaR2p.png)

Instead of writing the same character at the end of the moves, we will just write an arbitrary character instead

### Arbitrary search
If we want to look for `n` amount of characters (useful when traversing through sections by looking for pipes), we can modify the Arbitrary move states so that it only progresses when we find a character that we want for `n` number of times

![image](https://hackmd.io/_uploads/r1mkn60na.png)

## State memory and tape memory (registers vs ram)
So far we should be familiar by now that we can store information in the state and the tape. Just like how modern CPUs have different mediums for memory and storage _(registers, RAM, caches, disk ...)_ with their specific use cases (slow-presistent access vs fast-ephemeral access), we can also say the same for state storage vs tape storage here.

We can treat state storage as fast-ephemeral storage as we will be constantly changing the contents and we cant store too big information inside it. We chose this over tape memory because to access tape memory, you will need to move to certain place everytime you want to read/write the memory. We can store things like conditionals and counters using state memory.

However, this makes tape memory suitable for slow-persistent access because we dont have the desire to read/write tape memory often, it can be used to store things which needs to be there for a long time; in the context of UTM, it would be things like current state of input machine and current symbol read from input machine

## Conditionals
State memory can also help us to create conditionals like switch statements. The way it works is that we have a initial switch state which would scan for each unique character, and then the switch states would then jump to a unique to_state, which will continue with their respective transitions.

![image](https://hackmd.io/_uploads/B1klpCRh6.png)

Of course, there are a lot more things we can do with state memory and tape memory manipulation which can be derived from the operations above

## Machine initialization
The first few states of your UTM should be some initialization operations. The reason we do this is because the **inputted tape does not have any extra characters for us** to be used as tape memory, but at the same time, we do not need to use every single information inside the input machine to be able to carry out execution. So the whole goal of initialization is to **remove unwanted information and then be able to initialize that removed so that it can be used as tape memory.**

We combine the tape and state manipulation steps earlier to acheive this.

After initialization, our tape should look like this
`~~~~~~~~A~|.|A|E|A.A.RA1A1RA+A+RA=B.LB1C=LB+E.LC1C1LC+D1RD1D1RD=E.R|_1+1=`, notice now that the alphabets are removed and replaced with blank `~` characters and the initial state is recorded as well.

> TODO visualization

## Machine execution
This will be the Fetch-Decode-Execute cycle for our machine. The steps you can take to acieve this once our machine is initialized are :

1. find the cursor `_` from while moving right
2. Once the cursor is found, read the character pointed by the cursor and store it in state memory. Carry the value over to our tape memory region and write the symbol there.
3. Traverse to our transition space and look for a transition that matches the current state and read character. Since we know that transitions are seperated by `5` characters each, we are able to manually traverse them while matching.
4. Once we are at the correct transition (`state` and `read_char` matched), we will record its `to_state`, `action` and `write_char` in state memory
5. We go back to the cursor `_` and based on our state memory values, we will overwrite the symbol at the cursor with `write_char` and display the cursors position in the direction of `action`.
6. Before we loop back again, we traverse to the tape memory region where we store the current state and overwrite with the value of `to_state`
7. Go back to step 1

> TODO visualization
