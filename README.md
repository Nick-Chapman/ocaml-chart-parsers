# chart-parse-combinators

Ocaml parser combinators based on Earley & CYK chart parsing algorithms.

Supports arbitrary CFG grammars: Left recursion and ambiguity is no
problem. With the `bind` combinator we even get context sensitive parsing.
Like other parser combinators we may embed semantic actions into the
grammar specification allowing easy construction of AST.
Errors are automatically detected at the first input token which leads
to no possible parse.

Examples:

* Arith: Standard arithmetic precedence example (with a litte bit of ambiguity added for fun by not fixing if `+` associates to the left or right)

* NLP: Natural language processing example showing the PP attachment ambiguity problem.
http://allthingslinguistic.com/post/52411342274/how-many-meanings-can-you-get-for-the-sentence-i

* Catalan: A very ambiguous grammar: the number of parses for input length N being the Nth Catalan number.

* Choose7: Another ambiguous grammar: the number of parses for input length N being the Nth element in the 7th row of Pascal's triangle.

The Catalan and Choose7 examples originate from the following fun blog posting about the Marpa parser:
http://jeffreykegler.github.io/Ocean-of-Awareness-blog/individual/2012/06/the-useful-the-playful-the-easy-the-hard-and-the-beautiful.html
