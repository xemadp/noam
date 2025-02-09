
## A simple parser in bison

This is (for now) just a basic parser ( along with lexer) I wrote in bison
to print out the Noam grammar rules that are reduced in each action.

I mainly just made this to fix any inconsistency that existed in the Noam grammar specification and to just get a gist of how it would be parsed by an LALR(1) parser generator.

![bloop](https://xemadp.github.io/blog/pics/noamflexII.png)


It is noteworthy to mention that 
there is *some* modification involved when writing out the grammar in bison/yacc format.
but it isn't anything that would cause problems.

## build

just do 

``` bash
bison -d translate.y
```
to make `translate.tab.c` and `translate.tab.h`.

then do 

``` bash
flex -o lex.yy.c lex.l 
```

to make `lex.yy.c`. 

All that is left to do is : 

```bash
gcc -Wall -g lex.yy.c translate.tab.c -o parser -lfl 
```

to create `parser`, which will look for `input.txt` in noam grammar, and print out reduced rules.


