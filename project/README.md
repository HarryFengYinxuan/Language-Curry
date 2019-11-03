# Import Info

We choose REPL and file of definition as part of our mix-in features. Following the instructions here: https://piazza.com/class/jr9fgrf7efv7j0?cid=1400, we can't test IO easily, but here are some instructions to run the repl and file of definition. We can only load definition files from repl for now.

The 2 screenshots in this folder, project-curry/project, are working exmaples. REPL can take normal curry language with good grammar to execute, it can also print the loggings. Special let (without the "in" part of "let .. = .. in .. ") is made to be a special syntax for REPL. In REPL, you can type "let a = 1", and then for the following line, type some ast or other "let"s. This is the same as typing "let a = 1 in " following the ast that you typed after in normal syntax. Except for special let and import, everything else works as the normal ast, eval, and parser would work.

The definition file is imported by typing "import " and the filename (without ".curry") in REPL. An example definition file is stored in this directory, project-curry/project, which contains special let expressions for each line. Importing from this file is the same as typing every line on the file to REPL.

Parser works really slowly when there are lots of let expressions. 

# Project

Follow the [instructions](INSTRUCTIONS.md), use this space to document your project for yourself and the graders.

## Names
Yinxuan Feng, 
Yuanchong Bian, 
Mengqiao Cai

## Summary
We plan to include additional features including: loading definition files(http://www.cs.bu.edu/fac/snyder/cs320/Lectures/Lecture19--%20Project%20Prep%202pptx.pdf on page 9), static type checking, and REPL. Yinxuan Feng will be mainly in charge of Parser and help show. Yuanchong Bian will be mainly in charge of part of the evaluations and testing. Mengqiao Cai will be mainly in charge of the part of the evaluations and monad structuring.
## Plan
First, we use a functional week-10 homework, and then improve the monads (eg. EnvUnsafe) to let it also handle logging. This should be done bt Apr. 24th. 

Second, we include all the ASTs that we need to consider. This should be done by Apr. 26th. 

Everyone will contribute to the aforementioned tasks. Then there will be three main tasks going on: 

1. writing tests for eval, 
2. writing eval, 
3. and writing parser. 
There should be done by Apr. 28th.

The additional features will be achieved by Apr. 29th. The next few days will be used to do more testing and debugging and going to office hours to ask for help.

Specific plan: (not chronological)(try to write documentations as comments I guess)
1. Test and debug week 10 hw (done by Apr. 23rd)
2. Add new types of data, floats, characters, strings, and lists, in Ast (done by Apr. 24th)
3. Add sensible error messages, probably just check every eval as we go (kind of done)
  3.1 Use and not declared with "$var not in scope" error (run-time or static? waiting for answer in piazza)
4. Achieve comments in parser
5. Parse normal list format and change show of list in Lang.
6. Improve EnvUnsafeMonad (done by Apr. 24th)
7. Write tests after the Wednesday lecture.
  7.1 eval test (done)
  7.2 parser test
  7.3 check test
8. Implement stdLib
9. Show for Ast and Val (showFullyParen and showPretty) (Val part finished by Apr. 24th)
10. Write run and exec functions (done by Apr. 24th)
11. Continue to support integers, Booleans and curried function types (functions of one argument) (I am not sure what does it mean)
12. Dynamic type-checking for expressions for all operators and predefined functions and for all types, and reporting of appropriate errors.
13. Parser enhencements: specific error messages with context, find the line and character where it went wrong (localize the error to which parser it occured in if possible), and find scope base on indentation.
