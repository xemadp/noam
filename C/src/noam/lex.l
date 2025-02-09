%{
#include "translate.tab.h"
%}

%option noyywrap
%option yylineno

digit  [0-9]
id		 [a-zA-Z][a-zA-Z0-9_]*
string \"[a-zA-Z0-9_\ ]*\"

%%

[\t\n] {/*ignore ws*/}
"Automaton" { return AUTOMATON_KW; }
"states" { return STATES_KW; }
"start" { return START_KW; }
"accept" { return ACCEPT_KW; }
"Verify" { return VERIFY_KW; }

"inputset" { return INPUTSET_KW; }
"transition" { return TRANSITION_KW; }
"on" { return ON_KW; }
"goto" { return GOTO_KW; }

"canreach" { return CANREACH_KW; }
"accepts" { return ACCEPTS_KW; }
"deterministic" { return DETERMINISTIC_KW; }
"isempty" { return ISEMPTY_KW; }
"equal" { return EQUAL_KW; }
"terminates" { return TERMINATES_KW; }

";" { return SEMICOLON; }
"{" { return LBRACE; }
"}" { return RBRACE; }
"," { return COMMA; }
":" { return COLON; }

{id} {
	yylval.str_val = strdup(yytext);
	return IDENTIFIER;
}

{string} {
	yylval.str_val = strdup(yytext);
	return STRING;
}

%%

