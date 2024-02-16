
import sys
import json
from pprint import pprint

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
class Write:
	def __init__(self, type, value):
		self.type = type
		self.value = value

	def __str__(self):
		if self.type != "Write":
			return f"<{self.type}>"
		return f"{self.value}"

# To state value 
class ToState:
	def __init__(self, type, value):
		self.type = type
		self.value = value

	def __str__(self):
		if self.type != "To_state":
			return self.type
		return f"{self.value}"

# Transition value
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
class State:
	def __init__(self, name, transitions):
		self.name = name
		self.transitions = transitions

	def __str__(self):
		res = ""
		res += f"{self.name}, transitions: \n"
		for transition in self.transitions:
			res += f"{transition}\n"
		return res

#######################
###     PROGRAM     ###
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
# Converts all Next state into actual states
def join_states:
	print("haha")

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
	state_range = jsonstr['states']
	output_alphabet = [blank, pipe, cursor, right_char, left_char] + sub_alphabet

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
	state_1 = State("state1", [s_transition, s_transition_3, s_transition_2])
	state_2 = State("state2", [m_transition])
	states_lst = [state_1, state_2]
	for state in states_lst:
		print(state)
	res = final_application(states_lst, "END")
	for state in res:
		print(state)
	# print(res)
	# pprint(sub_alphabet[0])

# Check if the script is run directly
if __name__ == "__main__":
	if len(sys.argv) != 2:
		print(f"Usage: python {sys.argv[0]} <machine json>")
		sys.exit(1)
	filepath = sys.argv[1]
	main(filepath)
