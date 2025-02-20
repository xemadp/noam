(ql:quickload '(:cl-lex))

(defpackage :noam-lexer
  (:use :cl :cl-lex)
  (:export 
    #:noam-lexer
   :AUTOMATON_KW :STATES_KW :START_KW :ACCEPT_KW :VERIFY_KW 
   :INPUTSET_KW :TRANSITION_KW :ON_KW :GOTO_KW :CANREACH_KW 
   :ACCEPTS_KW :DETERMINISTIC_KW :ISEMPTY_KW :EQUAL_KW :TERMINATES_KW 
   :SEMICOLON :LBRACE :RBRACE :COMMA :COLON 
   :IDENTIFIER :STRING))
    

(in-package :noam-lexer)

;;Noam-lexer
(define-string-lexer noam-lexer
  
  ;; Ignore \t and newlines.
  ("[\\t\\n]+" nil)  
  
  
  ;; Reserved keywords.
  ("accepts"           (return (values 'ACCEPTS_KW $@)))
  ("Automaton"         (return (values 'AUTOMATON_KW $@)))
  ("states"            (return (values 'STATES_KW $@)))
  ("start"             (return (values 'START_KW $@)))
  ("accept"            (return (values 'ACCEPT_KW $@)))
  ("Verify"            (return (values 'VERIFY_KW $@)))
  ("inputset"          (return (values 'INPUTSET_KW $@)))
  ("transition"        (return (values 'TRANSITION_KW $@)))
  ("on"                (return (values 'ON_KW $@)))
  ("goto"              (return (values 'GOTO_KW $@)))
  ("canreach"          (return (values 'CANREACH_KW $@)))
  ("deterministic"     (return (values 'DETERMINISTIC_KW $@)))
  ("isempty"           (return (values 'ISEMPTY_KW $@)))
  ("terminates"        (return (values 'TERMINATES_KW $@)))
  ("equal"             (return (values 'EQUAL_KW $@)))

  (";"                 (return (values 'SEMICOLON $@)))
  (","                 (return (values 'COMMA $@)))
  ("{"                 (return (values 'LBRACE $@)))
  ("}"                 (return (values 'RBRACE $@)))
  (":"                 (return (values 'COLON $@)))


  ;; STRING
  ("\\\"[a-zA-Z0-9_ ]*\\\"" 
   (return (values 'STRING $@)))

  ;; IDENTIFIER
  ("[a-zA-Z][a-zA-Z0-9_]*" 
   (return (values 'IDENTIFIER $@)))

  )

;; Function to tokenize a given string
(defun tokenize-string (input)
  "Tokenize an input stream using the defined lexer."
  (let ((lexer (noam-lexer input)))
    (loop
       for token = (funcall lexer)
       while token
       do (format t "~A~%" token))))

(defun tokenize-file (filename)
  "Tokenize the contents of a file using the defined lexer."
  (let ((input (uiop:read-file-string filename))) ;; Read the entire file as a string
    (let ((lexer (noam-lexer input)))
      (loop
        for token = (funcall lexer)
        while token
        do (format t "~A~%" token)))))

