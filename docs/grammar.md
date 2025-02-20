program = { automaton_declaration | verification_declaration };

automaton_declaration = "Automaton" identifier "{"
state_declaration
start_state
accept_states
alphabet_declaration 
transition_declaration

"}";

state_declaration = "states" "{" {state, ","} state "}", ";";
start_state = "start" state, ";";
accept_states = "accept" "{" { state, "," } state "}", ";";
state = identifier;

alphabet_declaration = "inputset" "{" {string, ","}, string "}", ";";

transition_declaration = dfa_transition;
dfa_transition = "transition" state, ":" { transition_rule }

transition_rule = "on" string,"," "goto" state, ";";

verification_declaration = "Verify" identifier "{"
property_list
"}";

property_list = { property, ";"};
property = reachable | acceptance | determinism | emptiness | equivalence;
reachable = "canreach" "{" { state, "," } state  "}";
acceptance = "accepts"  "{" { string,"," } string "}";
determinism = "deterministic";
emptiness = "isempty";
equivalence = "equal" "{" { identifier,"," } identifier "}" ;
termination = "terminates" "{" { string, "," } string "}";

identifier = letter , { letter | digit | "_" } ;

letter =  "A" | "B" | "C" | "D" | "E" | "F" | "G"
       | "H" | "I" | "J" | "K" | "L" | "M" | "N"
       | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
       | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
       | "c" | "d" | "e" | "f" | "g" | "h" | "i"
       | "j" | "k" | "l" | "m" | "n" | "o" | "p"
       | "q" | "r" | "s" | "t" | "u" | "v" | "w"
       | "x" | "y" | "z";

digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;

symbol = "[" | "]" | "{" | "}" | "(" | ")" | "<" | ">"
       | "'" | '"' | "=" | "|" | "." | "," | ";" | "-" 
       | "+" | "*" | "?" | "\n" | "\t" | "\r" | "\f" | "\b" ;

string = { character };
character = letter | digit | symbol | "_" | " " ;
