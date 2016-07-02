
Ocaml parser combinators for CFG grammars. Supports left recursion and ambiguity.
Implemention based on chart parsing techniques (Earley / CYK) but with a functional programming twist!

Examples:

* Arithmetic expressions : standard predicence example

* Catalan: very ambiguous grammar (any binary tree): the number of parses for input length N being the Nth Catalan number

* Choose7: another ambiguous grammar: the number of parses for input length N being the Nth element in the 7th row of Pascal's triangle.
