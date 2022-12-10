# Open questions

## Exercise 4

The documentation for Happy mentions that using left-recursion results in a more
efficient parser than when using right-recursion.
Specifically, using left-recursion results in a parser that uses constant stack
space, while using right-recursion requires stack space proportional to the
amount of tokens being parsed for that production.

This is interesting because for parser combinators we explicitly don't want to
use left-recursion because it results in a looping parser.

## Exercise 10