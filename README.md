# Parser-Flecha

## How use it

 - Install [sbt](https://www.scala-sbt.org/1.0/docs/Setup.html) 
 - Run `git clone https://gitlab.com/trimegisto/parser-flecha`
 - Run `sbt`
 - Run scala console
 - Run ` Parser.parse(path_of_input, indent_spaces)`

## Example:

#### We have a file called test01.input with the next program:   

```
-- Numeros
def uno = 1
def dos =2--comentario
def tres= 3  -- otro comentario
def
cuatro=4--comentario
def cinco = 5 def seis = 6def siete = 7
  def
    ocho
      =
         8 def
nueve
=9
def cero=0
def cerocero=00
def cerocerocero=000
def def_=10
def ifthenelse=11
def p_r_u_e_b_a=1987654321
def camelCase=12
def x1 = 11
def x2 = 12

```
 
 #### We open the scala console and run:
 
 ` scala> Parser.parse("src/utils/testCases/input/test01.input", 2)`
 
```
[
  [
    "Def",
    "uno",
    [
      "ExprNumber",
      1
    ]
  ],
  [
    "Def",
    "dos",
    [
      "ExprNumber",
      2
    ]
  ],
  [
    "Def",
    "tres",
    [
      "ExprNumber",
      3
    ]
  ],
  [
    "Def",
    "cuatro",
    [
      "ExprNumber",
      4
    ]
  ],
  [
    "Def",
    "cinco",
    [
      "ExprNumber",
      5
    ]
  ],
  [
    "Def",
    "seis",
    [
      "ExprNumber",
      6
    ]
  ],
  [
    "Def",
    "siete",
    [
      "ExprNumber",
      7
    ]
  ],
  [
    "Def",
    "ocho",
    [
      "ExprNumber",
      8
    ]
  ],
  [
    "Def",
    "nueve",
    [
      "ExprNumber",
      9
    ]
  ],
  [
    "Def",
    "cero",
    [
      "ExprNumber",
      0
    ]
  ],
  [
    "Def",
    "cerocero",
    [
      "ExprNumber",
      0
    ]
  ],
  [
    "Def",
    "cerocerocero",
    [
      "ExprNumber",
      0
    ]
  ],
  [
    "Def",
    "def_",
    [
      "ExprNumber",
      10
    ]
  ],
  [
    "Def",
    "ifthenelse",
    [
      "ExprNumber",
      11
    ]
  ],
  [
    "Def",
    "p_r_u_e_b_a",
    [
      "ExprNumber",
      1987654321
    ]
  ],
  [
    "Def",
    "camelCase",
    [
      "ExprNumber",
      12
    ]
  ],
  [
    "Def",
    "x1",
    [
      "ExprNumber",
      11
    ]
  ],
  [
    "Def",
    "x2",
    [
      "ExprNumber",
      12
    ]
  ]
]
    
```

#### Also, we will find a new file called `test01.input.AST` within `src/utils/testCases/input/` with the same indent spaces