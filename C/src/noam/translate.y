%{
#include <stdlib.h>
#include <string.h>
#include  <stdio.h>

extern int yylex();
extern int yylineno;
extern char* yytext;
extern FILE* yyin;
FILE* output;

void yyerror(const char* s);



void print_reduction(char* reduction){
		fprintf(output, "%s", reduction);
}

%}

%union {
	char* str_val;
}


%token AUTOMATON_KW STATES_KW START_KW ACCEPT_KW VERIFY_KW
%token INPUTSET_KW TRANSITION_KW ON_KW GOTO_KW 
%token CANREACH_KW ACCEPTS_KW DETERMINISTIC_KW ISEMPTY_KW EQUAL_KW TERMINATES_KW
%token SEMICOLON LBRACE RBRACE COMMA COLON
%token <str_val> IDENTIFIER STRING

%start program

%%

program 
			: declaration_list
			{
					print_reduction("program -> declaration_list\n");
			}
			;

declaration 
			: automaton_declaration 
			{
					print_reduction("declaration -> automaton_declaration\n");
			}
			| verification_declaration
			{
					print_reduction("declaration -> verification_declaration\n");
			}
			;
	
declaration_list 
			: declaration declaration_list
			{
					print_reduction("declaration_list -> declaration declaration_list\n");
			}
			| /* nothing */
			{
					print_reduction("declaration_list -> epsilon\n");
			}
			;

automaton_declaration 
			: AUTOMATON_KW IDENTIFIER LBRACE state_declaration start_state accept_states alphabet_declaration transition_declaration_list RBRACE
			{
					print_reduction("automaton_declaration -> AUTOMATON_KW IDENTIFIER LBRACE state_declaration start_state accept_states alphabet_declaration transition_declaration_list RBRACE \n");
			}
			;

state_declaration
			: STATES_KW LBRACE identifier_list RBRACE SEMICOLON
			{
				print_reduction("state_declaration -> STATES_KW LBRACE identifier_list RBRACE SEMICOLON\n");
			}
			;

start_state
			: START_KW IDENTIFIER SEMICOLON
			{
			  print_reduction("start_state -> START_KW IDENTIFIER SEMICOLON\n");
			}
			;

accept_states 
			: ACCEPT_KW LBRACE identifier_list RBRACE SEMICOLON
			{
			  print_reduction("accept_states -> ACCEPT_KW LBRACE identifier_list RBRACE\n");
			}
			;

alphabet_declaration
			: INPUTSET_KW LBRACE string_list RBRACE SEMICOLON
			{
			print_reduction("alphabet_declaration ->  INPUTSET_KW LBRACE string_list RBRACE SEMICOLON\n");
			}
			;

transition_declaration_list
			: transition_declaration transition_declaration_list 
			{
			print_reduction("transition_declaration_list -> transition_declaration transition_declaration_list\n");
			}
			| transition_declaration
			{
			print_reduction("transition_declaration_list -> transition_declaration\n");
			}
			;

transition_declaration
			: dfa_transition
			{
			print_reduction("transition_declaration -> dfa_transition\n");
			}
			;

dfa_transition
			: TRANSITION_KW IDENTIFIER COLON transition_rule_list
			{
			print_reduction("dfa_transition -> TRANSITION_KW IDENTIFIER COLON transition_rule_list\n");
			}
			;

transition_rule 
			: ON_KW STRING COMMA GOTO_KW IDENTIFIER SEMICOLON
			{
			print_reduction("transition_rule -> ON_KW STRING COMMA GOTO_KW IDENTIFIER SEMICOLON\n");
			}
			;

verification_declaration 
			: VERIFY_KW IDENTIFIER LBRACE property_list RBRACE
			{
			print_reduction("verification_declaration -> VERIFY_KW IDENTIFIER LBRACE property_list RBRACE\n");
			}
			;

property_list 
			: property SEMICOLON property_list
			{
			print_reduction("property_list -> property SEMICOLON property_list\n");
			}
			| /*empty*/
			{
			print_reduction("property_list -> epsilon\n");
			}
			;

property 
			: reachable
			{
			print_reduction("property -> reachable\n");
			}
			| acceptance
			{
			print_reduction("property -> acceptance\n");
			}
			| determinism
			{
			print_reduction("property -> determinism\n");
			}
			| emptiness
			{
			print_reduction("property -> emptiness\n");
			}
			| equivalence
			{
			print_reduction("property -> equivalence\n");
			}
			| termination
			{
			print_reduction("property -> termination\n");
			}
			| /**/
			;

reachable
			: CANREACH_KW LBRACE identifier_list RBRACE
			{
			print_reduction("reachable -> CANREACH_KW LBRACE identifier_list RBRACE\n");
			}
			;

acceptance
			: ACCEPTS_KW LBRACE string_list RBRACE
			{
			print_reduction("acceptance -> ACCEPTS_KW LBRACE string_list RBRACE\n");
			}
			;

determinism
			: DETERMINISTIC_KW
			{
			print_reduction("determinism -> DETERMINISTIC_KW\n");
			}
			;

emptiness
			: ISEMPTY_KW
			{
			print_reduction("emptiness -> ISEMPTY_KW\n");
			}
			;

equivalence
			: EQUAL_KW LBRACE identifier_list RBRACE
			{
			print_reduction("equivalence -> EQUAL_KW LBRACE identifier_list RBRACE\n");
			}
			;

termination
			: TERMINATES_KW LBRACE string_list RBRACE
			{
			print_reduction("termination -> TERMINATES_KW LBRACE string_list RBRACE\n");
			}
			;

string_list 
			: STRING COMMA string_list
			{
			print_reduction("string_list -> STRING COMMA string_list\n");
			}
			| STRING
			{
			print_reduction("string_list -> STRING\n");
			}
			;

identifier_list 
			: IDENTIFIER COMMA identifier_list
			{
			print_reduction("identifier_list -> IDENTIFIER COMMA identifier_list\n");
			}
			| IDENTIFIER
			{
			print_reduction("identifier_list -> IDENTIFIER\n");
			}
			;

transition_rule_list
			: transition_rule transition_rule_list
			{
			print_reduction("transition_rule_list -> transition_rule transition_rule_list\n");
			}
			| transition_rule
			{
			print_reduction("transition_rule_list -> transition_rule\n");
			}
			;

%%

void yyerror(const char* s) {
    fprintf(stderr, "Error at line %d: %s near token '%s'\n", 
            yylineno, s, yytext);
}

int main() {
    // Open input file
    yyin = fopen("input.txt", "r");
		output = fopen("output.txt", "w");

    if (!yyin) {
        fprintf(stderr, "Error: Cannot open input file 'input.txt'\n");
        return 1;
    }

    // Parse input
    int result = yyparse();
	

	return 0;
}
