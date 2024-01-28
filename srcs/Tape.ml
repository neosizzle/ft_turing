open Types

let create (machine_definition:Types.machine_definition) input = 
  {
    blank = machine_definition.blank;
    data = input;
    head = 0;
    size = String.length input
  }

let read_head tape = String.get tape.data tape.head
let read_headi tape i = String.get tape.data i
