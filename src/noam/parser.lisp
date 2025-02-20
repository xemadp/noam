(ql:quickload '(:yacc))

;; Load the lexer file
(load "lexer.lisp")

;; Define the parser package
(defpackage :noam-parser
  (:use :cl :yacc :noam-lexer)
  (:export :noam-print-reductions))

(in-package :noam-parser)

(define-parser *noam-print-reductions* ;;(:print-states T)
  (:start-symbol program)
  (:terminals (AUTOMATON_KW STATES_KW START_KW ACCEPT_KW VERIFY_KW
               INPUTSET_KW TRANSITION_KW ON_KW GOTO_KW 
               CANREACH_KW ACCEPTS_KW DETERMINISTIC_KW ISEMPTY_KW EQUAL_KW TERMINATES_KW 
               SEMICOLON LBRACE RBRACE COMMA COLON
               IDENTIFIER STRING)) 

(program
  (declist (lambda (token)
               (format t "program -> declist~%")
               )))

(dec
 (automaton-declaration (lambda (token)
             (format t "dec -> automaton_declaration~%")
             ))
 (verification-declaration (lambda (token)
             (format t "dec -> verification_declaration~%")
             )))

(declist
  (dec declist (lambda (tx ty)
             (format t "declist -> dec declist~%")
             ))

  ((lambda ()
             (format t "declist -> epsilon~%")
             ))
)

(automaton-declaration
 (AUTOMATON_KW IDENTIFIER LBRACE 
  state-declaration 
  start-state 
  accept-states 
  alphabet-declaration 
  transition-declaration-list 
  RBRACE 
  (lambda (automaton-kw id lbrace state-dec start accept alphabet trans rbrace)
    (format t "automaton_declaration -> AUTOMATON_KW IDENTIFIER LBRACE state_declaration start_state accept_states alphabet_declaration transition_declaration_list RBRACE~%"))))

(state-declaration
 (STATES_KW LBRACE identifier-list RBRACE SEMICOLON 
  (lambda (states-kw lbrace ids rbrace semicolon)
    (format t "state_declaration -> STATES_KW LBRACE identifier_list RBRACE SEMICOLON~%"))))

(start-state
 (START_KW IDENTIFIER SEMICOLON
  (lambda (start-kw id semicolon)
    (format t "start_state -> START_KW IDENTIFIER SEMICOLON~%"))))

(accept-states
 (ACCEPT_KW LBRACE identifier-list RBRACE SEMICOLON
  (lambda (accept-kw lbrace ids rbrace semicolon)
    (format t "accept_states -> ACCEPT_KW LBRACE identifier_list RBRACE SEMICOLON~%"))))

(alphabet-declaration
 (INPUTSET_KW LBRACE string-list RBRACE SEMICOLON
  (lambda (inputset-kw lbrace strings rbrace semicolon)
    (format t "alphabet_declaration -> INPUTSET_KW LBRACE string_list RBRACE SEMICOLON~%"))))

(transition-declaration-list
 (transition-declaration transition-declaration-list 
  (lambda (trans-dec trans-list)
    (format t "transition_declaration_list -> transition_declaration transition_declaration_list~%")))

 (transition-declaration 
  (lambda (trans-dec)
    (format t "transition_declaration_list -> transition_declaration~%"))))

(transition-declaration
 (dfa-transition 
  (lambda (dfa-trans)
    (format t "transition_declaration -> dfa_transition~%"))))

(dfa-transition
 (TRANSITION_KW IDENTIFIER COLON transition-rule-list
  (lambda (trans-kw id colon rules)
    (format t "dfa_transition -> TRANSITION_KW IDENTIFIER COLON transition_rule_list~%"))))

(transition-rule
 (ON_KW STRING COMMA GOTO_KW IDENTIFIER SEMICOLON
  (lambda (on-kw str comma goto-kw id semicolon)
    (format t "transition_rule -> ON_KW STRING COMMA GOTO_KW IDENTIFIER SEMICOLON~%"))))

(verification-declaration
 (VERIFY_KW IDENTIFIER LBRACE property-list RBRACE
  (lambda (verify-kw id lbrace props rbrace)
    (format t "verification_declaration -> VERIFY_KW IDENTIFIER LBRACE property_list RBRACE~%"))))

(property-list
 (property SEMICOLON property-list
  (lambda (prop semicolon prop-list)
    (format t "property_list -> property SEMICOLON property_list~%")))
  (property SEMICOLON  ; 
  (lambda (prop semicolon)
    (format t "property_list -> property SEMICOLON~%")))

  (() ; 
  (lambda ()
    (format t "property_list -> epsilon~%"))))


(property
 (reachable 
  (lambda (reach)
    (format t "property -> reachable~%")))
 (acceptance 
  (lambda (accept)
    (format t "property -> acceptance~%")))
 (determinism 
  (lambda (det)
    (format t "property -> determinism~%")))
 (emptiness 
  (lambda (empty)
    (format t "property -> emptiness~%")))
 (equivalence 
  (lambda (equiv)
    (format t "property -> equivalence~%")))
 (termination 
  (lambda (term)
    (format t "property -> termination~%"))))

(reachable
 (CANREACH_KW LBRACE identifier-list RBRACE
  (lambda (canreach-kw lbrace ids rbrace)
    (format t "reachable -> CANREACH_KW LBRACE identifier_list RBRACE~%"))))

(acceptance
 (ACCEPTS_KW LBRACE string-list RBRACE
  (lambda (accepts-kw lbrace strings rbrace)
    (format t "acceptance -> ACCEPTS_KW LBRACE string_list RBRACE~%"))))

(determinism
 (DETERMINISTIC_KW
  (lambda (det-kw)
    (format t "determinism -> DETERMINISTIC_KW~%"))))

(emptiness
 (ISEMPTY_KW
  (lambda (empty-kw)
    (format t "emptiness -> ISEMPTY_KW~%"))))

(equivalence
 (EQUAL_KW LBRACE identifier-list RBRACE
  (lambda (equal-kw lbrace ids rbrace)
    (format t "equivalence -> EQUAL_KW LBRACE identifier_list RBRACE~%"))))

(termination
 (TERMINATES_KW LBRACE string-list RBRACE
  (lambda (terminates-kw lbrace strings rbrace)
    (format t "termination -> TERMINATES_KW LBRACE string_list RBRACE~%"))))

(string-list
 (STRING COMMA string-list
  (lambda (str comma str-list)
    (format t "string_list -> STRING COMMA string_list~%")))
 (STRING 
  (lambda (str)
    (format t "string_list -> STRING~%"))))

(identifier-list
 (IDENTIFIER COMMA identifier-list
  (lambda (id comma id-list)
    (format t "identifier_list -> IDENTIFIER COMMA identifier_list~%")))
 (IDENTIFIER 
  (lambda (id)
    (format t "identifier_list -> IDENTIFIER~%"))))

(transition-rule-list
 (transition-rule transition-rule-list
  (lambda (rule rule-list)
    (format t "transition_rule_list -> transition_rule transition_rule_list~%")))

 (transition-rule
  (lambda (rule)
    (format t "transition_rule_list -> transition_rule~%"))))

)

(defun parse-noam-file (filename)
  "Parse a Noam file and write reductions to reductions.txt"
  (with-open-file (output-stream "reductions.txt" 
                                :direction :output 
                                :if-exists :supersede 
                                :if-does-not-exist :create)
    ;; Temporarily bind *standard-output* to our file stream
    (let ((*standard-output* output-stream))
      (with-open-file (input-stream filename :direction :input)
        (let* ((file-contents (make-string (file-length input-stream)))
               (read-chars (read-sequence file-contents input-stream)))
          (parse-with-lexer 
           (noam-lexer (subseq file-contents 0 read-chars))
           *noam-print-reductions*))))))

