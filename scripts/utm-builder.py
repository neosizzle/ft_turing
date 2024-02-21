
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
# takes an array of state arrays [[s1, s2, s3], [s3, s5, s6]]
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
# }]
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

# reads json file and return the contents
def load_json_file(filepath):
	with open(filepath) as f:
		d = json.load(f)
		return d


def main(filepath):
	# open json
	jsonstr = load_json_file(filepath)
	
	# init globals
	sub_alphabet = jsonstr['alphabet']
	# state_range = jsonstr['states']
	state_range = list(map(lambda x: chr(x[0] + ord('A')), enumerate(jsonstr['states'])))
	global output_alphabet
	output_alphabet = state_range + [blank, pipe, cursor, right_char, left_char] + sub_alphabet

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
	state_1 = State("state1", [s_transition_3, s_transition_3])
	state_2 = State("state2", [m_transition])
	state_3 = State("state3", [m_transition])
	states_lst = [state_1, state_1]
	states_lst_nonext = [state_3, state_2]
	res = create_loop([state_1, State("name1", [s_transition_e, s_transition_e]), state_2])
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
