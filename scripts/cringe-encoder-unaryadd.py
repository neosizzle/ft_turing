s = '''
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
		"scanright",
		"addone",
		"replaceone",
		"end",
		"HALT"
	],
	"initial": "scanright",
	"finals": [
		"HALT"
	],
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
				"read": "+",
				"to_state": "scanright",
				"write": "+",
				"action": "RIGHT"
			},
			{
				"read": "=",
				"to_state": "addone",
				"write": ".",
				"action": "LEFT"
			}
		],
		"addone": [
			{
				"read": "1",
				"to_state": "replaceone",
				"write": "=",
				"action": "LEFT"
			},
			{
				"read": "+",
				"to_state": "HALT",
				"write": ".",
				"action": "LEFT"
			}
		],
		"replaceone": [
			{
				"read": "1",
				"to_state": "replaceone",
				"write": "1",
				"action": "LEFT"
			},
			{
				"read": "+",
				"to_state": "end",
				"write": "1",
				"action": "RIGHT"
			}
		],
		"end" : [
			{
				"read": "1",
				"to_state": "end",
				"write": "1",
				"action": "RIGHT"
			},
			{
				"read": "=",
				"to_state": "HALT",
				"write": ".",
				"action": "RIGHT"
			}
		]
	}
}
'''
user_input = input()
for char in s[:-1]:
    print('-', end='')
print('@', end='')
print(user_input)