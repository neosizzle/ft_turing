
import sys
import json

# reads json file and return the contents
def load_json_file(filepath):
	with open(filepath) as f:
		d = json.load(f)
		return d


def main(filepath):
	# open json
	jsonstr = load_json_file(filepath)
	
	binary = ""

	og_state_range = jsonstr['states']
	new_state = "".join(list(map(lambda x: chr(x[0] + ord('A')), enumerate(jsonstr['states']))))
	
	symbols = "".join(jsonstr['alphabet'])
	blank = jsonstr['blank'][0]

	initial_index = jsonstr['states'].index(jsonstr['initial'])

	final_indices = []
	for final in jsonstr['finals']:
		final_indices.append(jsonstr['states'].index(final))
	finals = ""
	for index in final_indices:
		finals += new_state[index]
	
	transition_string = ""
	for _transition in jsonstr['transitions'] :
		state = jsonstr['transitions'][_transition]
		for transition in state:
			state_idx = jsonstr['states'].index(_transition)
			curr_state = new_state[state_idx]

			to_state_idx = jsonstr['states'].index(transition['to_state'])
			to_state = new_state[to_state_idx]

			read = transition['read']
			write = transition['write']
			action = transition['action'][0]
			transition_string+= f"{curr_state}{read}{to_state}{write}{action}"
	print(f"{symbols}|{blank}|{new_state}|{new_state[initial_index]}|{finals}|{transition_string}|_{sys.argv[2]}")

# Check if the script is run directly
if __name__ == "__main__":
	if len(sys.argv) != 3:
		print(f"Usage: python {sys.argv[0]} <machine json> <input>")
		sys.exit(1)
	filepath = sys.argv[1]
	main(filepath)
