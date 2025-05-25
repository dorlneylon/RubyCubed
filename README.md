# RubyCubed — Simplified Ruby compiler for Risc-V.

## Usage:

```
❯ ./ruby_compiler

Usage: ruby_compiler <input_file> <output_file>
    <input_file>  - Source Ruby file to compile\n
    <output_file> - Output assembly file
```

## Example:

### Source code:

```rb
# playground/arithm.rb
r = 5 * (10 + 2) / 2

if r == 2
    3.times do
        r = r * r
    end
    puts r
else
    puts "unsure"
    puts r
end
```

### Compilation

```
❯ ./ruby_compiler ../playground/arithm.rb ../playground/arithm.S

=== Abstract Syntax Tree ===
Program
  VariableDeclaration(name="r")
    BinaryExpression(operator="/")
      BinaryExpression(operator="*")
        IntegerLiteral(5)
        BinaryExpression(operator="+")
          IntegerLiteral(10)
          IntegerLiteral(2)
      IntegerLiteral(2)
  IfStatement
    Condition:
      BinaryExpression(operator="==")
        Identifier("r")
        IntegerLiteral(2)
    Consequence:
      BlockStatement
        TimesStatement
          Count:
            IntegerLiteral(3)
          Body:
            BlockStatement
              VariableDeclaration(name="r")
                BinaryExpression(operator="*")
                  Identifier("r")
                  Identifier("r")
        ExpressionStatement
          CallExpression
            Identifier("puts")
            Arguments:
              Identifier("r")
    Alternative:
      BlockStatement
        ExpressionStatement
          CallExpression
            Identifier("puts")
            Arguments:
              StringLiteral("unsure")
        ExpressionStatement
          CallExpression
            Identifier("puts")
            Arguments:
              Identifier("r")
===========================

Compilation successful. Assembly written to '../playground/arithm.S'
```

### Result

```S
# playground/arithm.S

_start:
    li x5, 5
    add x7, x5, x0
    li x5, 10
    li x6, 2
    add x5, x5, x6
    add x6, x5, x0
    add x5, x7, x0
    mul x5, x5, x6
    li x6, 2
    div x5, x5, x6
    sw x3, 25000, x5
    lw x5, x3, 25000
    li x6, 2
    seq x5, x5, x6
    beq x5, x0, else_0
    li x5, 3
    li x6, 0
times_loop_start_2:
    bge x6, x5, times_loop_end_3
    lw x5, x3, 25000
    add x7, x5, x0
    lw x5, x3, 25000
    add x6, x5, x0
    add x5, x7, x0
    mul x5, x5, x6
    sw x3, 25000, x5
    addi x6, x6, 1
    jal x0, times_loop_start_2

# and on...
```

For more examples, see [playground](playground/).

# Installation

### Compiler:

```
mkdir build
cd ./build
cmake ..
make ruby_compiler
```

### Tests

```
make compiler_tests
./compiler_tests
```
