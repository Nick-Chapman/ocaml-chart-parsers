# chart-parse-combinators

Ocaml parser combinators for CFG grammars. Supports left recursion and ambiguity.
Implementation based on chart parsing techniques (Earley / CYK) but with a functional programming twist!

Examples:

* Arith: The standard arithmetic precedence example for bin-ops `+` and `*`

* Catalan: A very ambiguous grammar: the number of parses for input length N being the Nth Catalan number.

* Choose7: Another ambiguous grammar: the number of parses for input length N being the Nth element in the 7th row of Pascal's triangle.
