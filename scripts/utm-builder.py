
import sys
import json
from pprint import pprint
import copy
# copy.deepcopy?


#######################
###     CLASSES     ###
#######################
class Alphabet:
	def __init__(self):
		self.inner = []

	def __str__(self):
		return f"{self.inner}"

# Left or right?
class Action:
	def __init__(self, direction):
		self.inner = direction

	def __str__(self):
		return f"{self.inner}"

# All states
states = []

# Write value
# type - Write (arbitrary string) | Copy (sme with read char in transiton)
# value - Write's string
class Write:
	def __init__(self, type, value):
		self.type = type
		self.value = value

	def __str__(self):
		if self.type != "Write":
			return f"<{self.type}>"
		return f"{self.value}"

# To state value
# type - To_state (arbitrary to state string) | Same (Same wtih current state) | Next (Another state, not known) | Loop (special type for loops)
# value - To_state's string
class ToState:
	def __init__(self, type, value):
		self.type = type
		self.value = value

	def __str__(self):
		if self.type != "To_state":
			return self.type
		return f"{self.value}"

# Transition value
# type - Standart | Multiple
# read_char - string to read or string(s) to read if with 'Multiple' type
# to_state - ToState struct
# write - string to write
# action - action struct
class Transition:
	def __init__(self, type, read_char, to_state, write, action):
		self.type = type
		self.read_char = read_char
		self.to_state = to_state
		self.write = write
		self.action = action

	def __str__(self):
		return f"{self.read_char}, to_state: {self.to_state}, write: {self.write}, action: {self.action}, type: {self.type}"

# State value
# name - state name
# transitions - array of Transition struct
class State:
	def __init__(self, name, transitions):
		self.name = name
		self.transitions = transitions

	def __str__(self):
		res = ""
		res += f"{{ {self.name}, transitions: \n"
		for index, transition in enumerate(self.transitions):
			res += f"{transition}\n"
		res += "}"
		return res

#######################
###     GLOBALS     ###
#######################

# absfun, an array which stores an array of (state classes)
absfun = []

# labsfun
labsfun = []

blank = '~'
pipe = "|"
cursor = "_"
right_char = "R"
left_char = "L"

state_range = []
sub_alphabet = []
output_alphabet = []

#######################
###     PROGRAM     ###
#######################

# prints store return value for debugging
def print_store(res):
	print("-------switch------")
	print(res[0])
	print("-------------------")

	for branches in res[1] :
		for branch in branches:
			print("==========seq=========")
			for sequence_state in branch[0]:
				print(sequence_state)
			print("======================")
			print("==========ext=========")
			for ext_state in branch[1]:
					print(ext_state)
			print("======================")
		print("======branch end=====")

# reverses an action value
def revserse_aaction(action):
	if action.direction == "LEFT" :
		action.direction = "RIGHT"
	else :
		action.direction = "LEFT"

# apply_absfun
# also populates transitions with 'multiple' 
# this populates write fields as well
# this should be called last because all Next to_states will be routed to final 
def final_application (_absfun, final_statename):
	res = []
	for state in _absfun:
		state_name = state.name

		# expand all multiple type transitions
		transitions_expanded = list(filter(lambda elem : elem.type == "Standart", state.transitions))
		for t in state.transitions :
			if t.type == "Multiple":
				for readchar in t.read_char :
					transitions_expanded.append(Transition("Standart", readchar, t.to_state, t.write, t.action))

		# convert tostate values of all transitions
		transitions_resovled = []
		for transition in transitions_expanded :
			write_resolved = Write("Write", transition.write.value if transition.write.type == "Write" else transition.read_char)

			match transition.to_state.type:
				case "To_state":
					transitions_resovled.append(Transition("Standart", transition.read_char, ToState("To_state", transition.to_state.value), write_resolved, transition.action))
				case "Same":
					transitions_resovled.append(Transition("Standart", transition.read_char, ToState("To_state", state_name), write_resolved, transition.action))
				case "Next":
					transitions_resovled.append(Transition("Standart", transition.read_char, ToState("To_state", final_statename), write_resolved, transition.action))
				case "Loop":
					raise ValueError('Loop still exists.')

		# create that new state to push in res
		new_state = State(state_name, transitions_resovled)

		# push to res
		res.append(new_state)
	return res

# join_absfun
# takes an array of state arrays [[s1, s2, s3], [s4, s5, s6]]
# any next to_states in transitions in s1, s2 or s3 will be converted to To_state to s4
# Converts all Next state into actual states
def join_states(state_lists):
	res = []
	for index, state_list in enumerate(state_lists):
		next_item = None
		if index < len(state_lists) - 1:
			next_item = state_lists[index + 1]
		for state in state_list :
			new_transitions = []
			for transition in state.transitions :
				new_transition = transition
				if transition.to_state.type == "Next" and next_item != None:
					new_transition = Transition(transition.type, transition.read_char, ToState("To_state", next_item[0].name), transition.write, transition.action)
				new_transitions.append(new_transition)
			new_state = State(state.name, new_transitions)
			res.append(new_state)
	return res

# nmoove
# generates arbitrary move states and transitions according to all output_characters
# the states generated are according to the len input
# <len>moove_<action>, <len - 1>moove_<action>, <len - 2>moove_<action> ... until <len - n> = 1
# the to_state value of the last state generated here will be a Loop or a Next type based on arg as well
def nmoove(len, action, loop = False, name = "moove_"):
	res = []
	for index in range(len,  0, -1):
		new_transition = Transition(
			"Multiple",
			copy.deepcopy(output_alphabet),
			ToState("To_state", f"{index - 1}{name}{action.inner}") if index > 1 else ToState(f"{'Loop' if loop else 'Next'}", ""),
			Write("Copy", ""),
			action,
			)
		new_state = State(f"{index}{name}{action.inner}", [new_transition])
		res.append(new_state)
	return res

# if_func
# takes in a condition list, which will be mapped to transitions of a state
# [{
# 	'read_char': '',
# 	'to_state': ToState(...),
# 	'action': Action(...)
# }...]
def if_func(condlist, name = "if_state"):
	new_transitions = []
	for cond in condlist :
		new_transitions.append(Transition("Standart", cond['read_char'], cond['to_state'], Write("Copy", ""), cond['action']))
	res = State(name, new_transitions)
	return [res]

# create_loop
# Takes in a list of states, mapping back the original list with 
# all the transitions which have a to_state type of Loop into a to_state
# type of To_state with the value as the first state in the list
def create_loop(states):
	if len(states) < 0 :
		raise ValueError('create_loop: states must be longer than 0')
	res = []
	first_statename = states[0].name
	for state in states :
		new_transitions = []
		for transition in state.transitions:
			new_transition = transition
			if transition.to_state.type == "Loop":
				new_transition = Transition(
					transition.type,
					transition.read_char,
					ToState("To_state", first_statename),
					transition.write,
					transition.action
					)
			new_transitions.append(new_transition)
		res.append(State(state.name, new_transitions))
	return res

# store
# make a switch-execute-and-write state sequence
# the first argument is a list of possible characters to switch
# the second argument is the states that need to be executed BEFORE the write
# the third argument is a function that accepts a name template which specifies which states to be executied FOR the write
# the 4th argument is the direction all of switches will take place
# this would generate a  state * ((state list * state list) list) in ocaml notation
# it would generate a [State, [ [ [State...], [State...] ] ...] ]. the notations with the '...' are variable length.
def store(character_list, states_pre_write, states_for_write_fn, switch_direction) :
	res = []

	if len(states_pre_write) < 0 :
		raise ValueError('create_loop: states must be longer than 0')
	
	# generates the switch state (res[0])
	switch_state_transitions = []
	for read_char in character_list :
		new_transition = Transition(
			"Standart",
			read_char,
			ToState("To_state", f"st_{read_char}_{states_pre_write[0].name}"),
			Write("Copy", ""),
			switch_direction
			)
		switch_state_transitions.append(new_transition)
	switch_state = State("st_switch", switch_state_transitions)

	# generates the branches depending with length of how
	# many items we have at char_list  (res[1])
	branches = []

	for read_char in character_list :
		# generate each branch, effectively a pair of state lists. res[1][...]
		state_list_pairs = []

		# generate the first list of the pair - the sequence for states_pre_write
		# for the current branch res[1][...][0][0]
		sequence_states = []
		for sequence_state in states_pre_write :
			new_statename = f"st_{read_char}_{sequence_state.name}"
			new_transitions = []
			for transition in sequence_state.transitions:
				new_transition = transition
				if new_transition.to_state.type == "To_state" :
					new_transition = Transition(
						transition.type,
						transition.read_char,
						ToState("To_state", f"st_{read_char}_{new_transition.to_state.value}"),
						transition.write,
						transition.action
					)
				new_transitions.append(new_transition)
			new_sequence_state = State(new_statename, new_transitions)
			sequence_states.append(new_sequence_state)

		# generate the second list of the pair - the extract states for current branch
		# res[1][...][0][1]
		extract_states = []
		pregen_write_states = states_for_write_fn(read_char)
		for state in pregen_write_states :
			# change state name
			new_statename = f"extract_{read_char}_{state.name}"
			new_transitions = []

			# change new transitions, replace tostate names
			for transition in state.transitions :
				new_transition = transition
				if new_transition.to_state.type == "To_state" :
					new_transition = Transition(
						transition.type,
						transition.read_char,
						ToState("To_state", f"extract_{read_char}_{new_transition.to_state.value}"),
						transition.write,
						transition.action
					)
				new_transitions.append(new_transition)
			new_state = State(new_statename, new_transitions)
			extract_states.append(new_state)

		state_list_pairs.append(sequence_states)
		state_list_pairs.append(extract_states)
		branches.append([state_list_pairs])

	res.append(switch_state)
	res.append(branches)

	return res

# store_fun
# joins all the states from the store() function
# for each branch, any to_state Next's on the sequenced states will be pointed to
# the first state of the paired extract state 
def join_store(store_ret):
	res = []

	# append the first switch state
	res.append(store_ret[0])

	# process branches
	for branch in store_ret[1]:
		seq_states = branch[0][0]
		ext_states = branch[0][1]
		joined_states = join_states([seq_states, ext_states])
		for state in joined_states :
			res.append(state)
	return res

# loop_after_switch
# does the same thing with join_store, except every to_state Loop in 
# ext_states of each branch is changed to To_state with the value of the pairs
# first seq_states 
def loop_after_switch(store_ret) :
	res = []

	# append the first switch state
	res.append(store_ret[0])

	# process branches
	for branch in store_ret[1]:
		seq_states = branch[0][0]
		ext_states = branch[0][1]
		new_ext_states = []
		for state in ext_states :
			new_transitions = []
			for transition in state.transitions :
				new_transition = transition
				if transition.to_state.type == "Loop":
					new_transition = Transition(
						transition.type,
						transition.read_char,
						ToState("To_state", seq_states[0].name),
						transition.write,
						transition.action
					)
				new_transitions.append(new_transition)
			new_state = State(state.name, new_transitions)
			new_ext_states.append(new_state)
		joined_states = join_states([seq_states, new_ext_states])
		for state in joined_states :
			res.append(state)
	return res

# loop_before_use
# similar to loop_after_swiitch but it the loop will now point to the first state 
# of ext_states instead
def loop_before_use(store_ret) :
	res = []

	# append the first switch state
	res.append(store_ret[0])

	# process branches
	for branch in store_ret[1]:
		seq_states = branch[0][0]
		ext_states = branch[0][1]
		new_ext_states = []
		for state in ext_states :
			new_transitions = []
			for transition in state.transitions :
				new_transition = transition
				if transition.to_state.type == "Loop":
					new_transition = Transition(
						transition.type,
						transition.read_char,
						ToState("To_state", ext_states[0].name),
						transition.write,
						transition.action
					)
				new_transitions.append(new_transition)
			new_state = State(state.name, new_transitions)
			new_ext_states.append(new_state)
		joined_states = join_states([seq_states, new_ext_states])
		for state in joined_states :
			res.append(state)
	return res

# write_c
# Creates an arbitrary write state
def arb_write(action, write_char) :
	return [State(f"write_{write_char}", [Transition(
		"Multiple",
		output_alphabet,
		ToState("Next", ""),
		Write("Write", write_char),
		action
	)])]

# range_without_c
# takes a list and a character, then filters out all occurrences of c from the list
def range_without_c(range, c):
    return [x for x in range if x != c]

# blank_until_char
# this will create a list of states that will write blank characters to the tape head
# until char is found.
# the ovreride char option is set to true if you want to override the destination char
def blank_until_char(char, action, override_char = False) :
	final_write = Write("Write", blank) if override_char else Write("Copy", "")
	transitions = [
		Transition("Standart", char, ToState("Next", ""), final_write, action),
		Transition("Multiple", range_without_c(output_alphabet, char), ToState("Same", ""), Write("Write", blank), action),
	]
	res = [
		State(f"blank_until_{char}", transitions)
	]
	return res

# find_nchar
# this will create a list of states that will call next once len number of 
# c has been found while traversing find_dir
def find_nchar(len, c, next_dir, find_dir, loop = False):
	res = []
	for i in range(len, 0, -1):
		is_final = True if i == 1 else False
		statename = f"{find_dir.inner[0]}find{i}{c}"
		next_statename = f"{find_dir.inner[0]}find{i - 1}{c}"
		found_to_state = ToState("Loop" if loop else "Next", "") if is_final else ToState("To_state", next_statename)
		transitions = [
			Transition("Standart", c, found_to_state, Write("Copy", ""), find_dir),
			Transition("Multiple", range_without_c(output_alphabet, c), ToState("Same", ""), Write("Copy", ""), next_dir),
		]
		res.append(State(statename, transitions))
	return res

# restruct_machine
# generates the states and transitions for the UTM initialization
def build_machine_init():
	actions = [
		blank_until_char(pipe, Action("RIGHT"), True),
		find_nchar(1, pipe, Action("RIGHT"), Action("RIGHT")),
		blank_until_char(pipe, Action("RIGHT")),
		find_nchar(2, pipe, Action("LEFT"), Action("LEFT")),
		join_store(
			store(
				sub_alphabet,
				join_states([find_nchar(2, pipe, Action("RIGHT"), Action("RIGHT")), nmoove(2, Action("LEFT"))]),
				(lambda c: arb_write(Action("LEFT"), c)),
				Action("LEFT")
				)
			),
		blank_until_char(pipe, Action("LEFT"), True),
		blank_until_char(blank, Action("RIGHT")),
		find_nchar(1, pipe, Action("RIGHT"), Action("LEFT")),
		nmoove(1, Action("LEFT")),
		arb_write(Action("RIGHT"), pipe),
		nmoove(2, Action("RIGHT")),
		join_store(
			store(
				state_range,
				join_states([nmoove(4, Action("LEFT"))]),
				(lambda c: arb_write(Action("RIGHT"), c)),
				Action("LEFT")
				)
			),
	]
	
	actions_indexed = []
	for index, action in enumerate(actions) :
		new_action = []
		for state in action:
			new_transitions = []
			for transition in state.transitions :
				new_transition = transition
				if transition.to_state.type == "To_state" :
					new_transition = Transition(
						transition.type,
						transition.read_char,
						ToState("To_state", f"{index + 1}_{transition.to_state.value}"),
						transition.write,
						transition.action
					)
				new_transitions.append(new_transition)
			new_state = State(f"{index + 1}_{state.name}", new_transitions)
			new_action.append(new_state)
		actions_indexed.append(new_action)

	res = join_states(actions_indexed)
	return res

# test_finals
# generates states that switches to HALT state upon reading input char
# also jumps to next state upon reading pipe
# only goes right
def test_finals(c) :
	new_statename = f"is_final_{c}"
	states_without_c = range_without_c(state_range, c)
	transitions = []

	transitions.append(Transition(
		"Standart",
		c,
		ToState("To_state", "HALT"),
		Write("Copy", ""),
		Action("RIGHT")
	))
	transitions.append(Transition(
		"Standart",
		pipe,
		ToState("Next", ""),
		Write("Copy", ""),
		Action("RIGHT")
	))
	for state_name in states_without_c:
		new_transition = Transition(
			"Standart",
			state_name,
			ToState("To_state", new_statename),
			Write("Copy", ""),
			Action("RIGHT")
			)
		transitions.append(new_transition)
	return State(new_statename, transitions)

# test_state_transition
# this will generate states that will switch to Next state when reading input machine states
# if c is found. Also moves RIGHT 4 times before doing the searching OR search current char.
def test_state_transition(c) :
	res = []
	states_without_c = range_without_c(state_range, c)

	# prepare cond
	cond = []
	cond.append({
		'read_char': c,
		'to_state': ToState("Next", ""),
		'action': Action("RIGHT")
	})
	cond.append({
		'read_char': pipe,
		'to_state': ToState("To_state", "Sub_undefined"),
		'action': Action("RIGHT")
	})
	cond_tostate = ToState("To_state", nmoove(4, Action("RIGHT"), name=f"{c}_state_check_moove")[0].name)
	cond_action = Action("RIGHT")
	for statename in states_without_c :
		cond_read = statename
		cond.append({
			'read_char': cond_read,
			'to_state': cond_tostate,
			'action': cond_action
		})

	check_transitions = if_func(cond, name=f"{c}_is_state_transition")
	moove_transitions = nmoove(4, Action("RIGHT"), loop=True, name=f"{c}_state_check_moove")

	# combine the states
	res = []
	for s in check_transitions:
		res.append(s)
	for s in moove_transitions:
		res.append(s)	
	
	return create_loop(res)

# test_read_transition
# this will generate states that will switch to Loop state when reading input machine alphabets
# if c is found. Also moves RIGHT 3 times before doing the searching OR search current char.
def test_read_transition(c) :
	res = []
	alphabets_without_c = range_without_c(sub_alphabet, c)

	# prepare cond
	cond = []
	cond.append({
		'read_char': c,
		'to_state': ToState("Next", ""),
		'action': Action("RIGHT")
	})
	cond_tostate = ToState("To_state", nmoove(3, Action("RIGHT"), name=f"{c}_transition_check_moove_")[0].name)
	cond_action = Action("RIGHT")
	for statename in alphabets_without_c :
		cond_read = statename
		cond.append({
			'read_char': cond_read,
			'to_state': cond_tostate,
			'action': cond_action
		})

	check_transitions = if_func(cond, name=f"{c}_is_read_transition")
	moove_transitions = nmoove(3, Action("RIGHT"), loop=True, name=f"{c}_transition_check_moove_")

	# combine the states
	res = []
	for s in check_transitions:
		res.append(s)
	for s in moove_transitions:
		res.append(s)	
	
	# loop here??
	return res

# find_transition
# generates states to find the transition given a certain character in the input machine
# this will generate branches?
def find_transition() :
	
	# hey we have encountered a character in the input tape
	# now we try to locate its transition function in registers
	
	# this function will take two arguments, the supposedly current read character and the current state
	# of the input machine, and it will generate the transitions to set the state memory AFTER input char is read
	# and call Next eventually when the correct memory is set based on the read character and the 
	# current state.

	# the return of this will be [switchTransitionForSt, [findStatesAndTransitions...]]
	def store_states(rd, st):
		res = []
		go_to_transition = find_nchar(4, pipe, Action("RIGHT"), Action("RIGHT"))
		find_trans = create_loop(join_states([test_state_transition(st), test_read_transition(rd)]))
		final_unformatted = join_states([go_to_transition, find_trans])

		# formatting function to format statenames and tostate values
		def format_statenames(state): 
			new_statename = f"st_{rd}_{st}_{state.name}"
			new_transitions = []
			for transition in state.transitions :
				new_tostate_name = f"st_{rd}_{st}_{transition.to_state.value}"
				if transition.to_state.type != "To_state":
					new_tostate_name = ""
				else :
					if transition.to_state.value == "HALT" or transition.to_state.value == "Sub_undefined":
						new_tostate_name = transition.to_state.value
				new_transition = Transition(
					transition.type,
					transition.read_char,
					ToState(transition.to_state.type, new_tostate_name),
					transition.write,
					transition.action
				)
				new_transitions.append(new_transition)
			return State(new_statename, new_transitions)

		final_fun = list(map(lambda state: format_statenames(state), final_unformatted))
		
		return [
			Transition(
				"Standart",
				st,
				ToState("To_state", final_fun[0].name),
				Write("Copy", ""),
				Action("RIGHT"),
			),
			final_fun
		]
	
	# helper function to generate store_states for rd char via machine input 
	# for all state ranges. The transitions generated here will run WHEN input char is read
	# as this function wraps the store_states outside of rd.

	# returns [switchTransitionForRd, [findStatesWithTransitions....]]
	def store_read(rd) :
		all_store_states = list(map(lambda st: store_states(rd, st), state_range))
		state_find_transitions_aft_read = []
		switch_transitions = []
		find_states = []
		res = []
		for switch_find_pair in all_store_states:
			switch_transition = switch_find_pair[0]
			_find_states = switch_find_pair[1]
			
			switch_transitions.append(switch_transition)
			for find_state in _find_states:
				find_states.append(find_state)
			# switch_statename = f"st{rd}st_switch"
			# state_find_transitions_aft_read.append(State(switch_statename, [switch_transition]))
			# for find_state in find_states:
			# 	state_find_transitions_aft_read.append(find_state)
		
		state_find_transitions_aft_read.append(State(f"st{rd}st_switch", switch_transitions))
		for find_state in find_states:
				state_find_transitions_aft_read.append(find_state)
		return [
			Transition(
				"Standart",
				rd,
				ToState("To_state", state_find_transitions_aft_read[0].name),
				Write("Copy", ""),
				Action("LEFT"),
			),
			state_find_transitions_aft_read
		]
	
	all_store_reads = list(map(lambda rd: store_read(rd), sub_alphabet))
	switch_transitions = []
	find_states = []
	res = []
	for switch_find_pair in all_store_reads:
		switch_transition = switch_find_pair[0]
		_find_states = switch_find_pair[1]
		
		switch_transitions.append(switch_transition)
		for find_state in _find_states:
			find_states.append(find_state)

	res.append(State("st_switch", switch_transitions))
	for state in find_states:
		res.append(state)
	return res

# reads json file and return the contents
def load_json_file(filepath):
	with open(filepath) as f:
		d = json.load(f)
		return d


def main(filepath):
	# open json
	jsonstr = load_json_file(filepath)
	
	# init globals
	global sub_alphabet
	sub_alphabet = jsonstr['alphabet']
	# state_range = jsonstr['states']
	global state_range
	state_range = list(map(lambda x: chr(x[0] + ord('A')), enumerate(jsonstr['states'])))
	global output_alphabet
	output_alphabet = state_range +  sub_alphabet + [cursor, pipe, left_char, right_char, blank]

	# testing
	l_action = Action("LEFT")
	r_action = Action("RIGHT")
	w_write = Write("Write", "some char")
	c_write = Write("Copy", "")
	t_to_state = ToState("To_state", "Some to state")
	s_to_state = ToState("Same", "")
	n_to_state = ToState("Next", "")
	l_to_state = ToState("Loop", "")
	s_transition = Transition("Standart", "read char", t_to_state, w_write, l_action)
	s_transition_2 = Transition("Standart", "readchar2", s_to_state, c_write, r_action)
	s_transition_3 = Transition("Standart", "readchar3", n_to_state, c_write, r_action)
	s_transition_e = Transition("Standart", "readchare", l_to_state, c_write, r_action) 
	m_transition = Transition("Multiple", ["read char", "m_read"], t_to_state, w_write, l_action) 
	state_1 = State("state1", [s_transition_3])
	state_2 = State("state2", [s_transition_e])
	state_3 = State("state3", [m_transition])
	states_lst = [state_1, state_1]
	states_lst_nonext = [state_3, state_2]
	def c_list(s) :
		return [State("whattt", [s_transition_2, s_transition_3]), State("howw", [s_transition, s_transition_e])]
	res_store = store(["=one", "=two", "=three"], [state_2, state_1, state_3], c_list, l_action)
	res = find_transition()
	for state in res:
		print(state)
	# pprint(sub_alphabet[0])

# Check if the script is run directly
if __name__ == "__main__":
	if len(sys.argv) != 2:
		print(f"Usage: python {sys.argv[0]} <machine json>")
		sys.exit(1)
	filepath = sys.argv[1]
	main(filepath)
