# chart-parse-combinators

Ocaml parser combinators based on Earley & CYK chart parsing algorithms.

Supports arbitrary CFG grammars: Left recursion and ambiguity is no
problem. With the `bind` combinator we even get context sensitive parsing.
Like other parser combinators we may embed semantic actions into the
grammar specification allowing easy construction of AST.
Errors are automatically detected at the first input token which leads
to no possible parse.

Examples:

* Arith: The standard arithmetic precedence example for bin-ops `+` and `*`

* Catalan: A very ambiguous grammar: the number of parses for input length N being the Nth Catalan number.

* Choose7: Another ambiguous grammar: the number of parses for input length N being the Nth element in the 7th row of Pascal's triangle.
