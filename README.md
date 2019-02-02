# C-- Compiler
C-- is a programming language much like C, but with a few features missing allowing for easier compilation. The language still includes the essentials such as: variables, loops, functions with arguments and return values, arrays.

# Hello World
Every program has a `main` function, which is run first. Using the `printc` command we can print out the character representation of a number. Since strings are null terminated, we can use a loop to iterate through each character, printing them out.

```c
def main() {
  let s = "Hello World!"

  let i = 0
  while s[i] != 0 {
    printc(s[i])
    i = i + 1
  }
}
```

# Variables
Variables can be used to store integers, or pointers to memory locations. The type is stored in determined using type inference.
For example, the snippet below stores a value `10` in the program's stack.

```c
def main() {
  let x = 10
}
```

This compiles to the assembly code below. Line 1 increments the stack pointer to make some space to store the variable. Line 2 moves the value 10 into the register 0. Line 3 takes this value and uses the `StoreIdx` instruction to store the value of a register to a memory address, i.e. the space made on the stack. Finally, Line 4 cleans up the stack by decrementing the stack pointer, and Line 5 exits the program.  

```
0| .main:
1|     ADDI sp sp #1
2|     MOVI r0 #10
3|     ST r0 bp #0
4|     SUBI sp sp #1
5|     EXIT
```

# Conditionals
The language supports if-else conditions to allow branched execution.

# Loops

# Arrays

# Function Calls
