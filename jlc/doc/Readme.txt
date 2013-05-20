The program is called by using this command :
./jlc -b JVM filename
./jlc -b LLVM filename

But because we still have trouble generating code for JVM, the LLVM
code generator is not ready.

We had trouble with conditions and OR expression in a return statement
so we changed quite a lot of things, making OR and AND expressions
jumping to boolean constant. Now we have labels problem.

To sum up, we need some more time to fix thoses problems. We need to
step back to what we had and change the RETURN generation plus the NOT generation.

Erwan CHAUSSY & Vincent BELLEC
