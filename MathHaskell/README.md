# Calculator Tool 
A Haskell-based calculator tool that performs mathematical calculations (tokenizes, parses and evaluates mathematical expressions), conversion functions (converts numbers between decimal, binary and hexadecimal), and equation solving (simple equations including variables, logarithms and arithmetic operations). 

## Features 
- **Mathematical Calculations** 
    Parses and tokenizes mathimatical expressions, supporting operators such as addition, substraction, multiplication, division, exponentiation and logarithms 

    - Tokenization : The program converts input strings into tokens representing numbers, variables and operators 

    - Parsing : Tokens are parsed into an AST (Abstract Syntax Tree) representing the mathematical expressions. 

    - Evaluation : The AST is evaluated recursively to compute the result of expressions or solve equations 

- **Conversions**
    Converts numbers between decimal, binary and hexadecimal formats 

- **Equation solving** 
    Solves linear equations, equations with logarithms and equations involving products and powers. 


### Examples 
- **Mathematical Expressions** : `sin(30) + 1`
- **Conversion** : Binary `100` to Decimal (`binaryToDecimal`)
- **Equation Solving** : Linear equation: `2x + 3 = 15` 

#### Examples Workflow 
- Mathematical Expressions 
    1- Choose option 1 
    2- Enter your expression (ex: 2+3*4)
    3- Output (ex: 14)

- Conversions 
    1- Choose option 2 
    2- Specify your conversion type (ex: binaryToDecimal)
    3- Enter the number (ex: 100)
    4- Output (ex: 4)

- Equation Solving 
    1- Choose option 3 
    2- Enter your equation (ex: 2x+3=15)
    3- Output (ex: x = 6.0)

##### Dependencies 
- Data.Char 
- Data.List 
- Numeric 
- Prelude 

###### Authors 
Freddy Chedid 
Thalia Atallah 