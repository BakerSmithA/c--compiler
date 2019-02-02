# C-- Compiler
C-- is a programming language much like C, but with a few features missing allowing for easier compilation. The language still includes the essentials such as: variables, loops, functions with arguments and return values, arrays. Example programs can be found [here](https://github.com/BakerSmithA/c--compiler/tree/master/examples/benchmark).

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

This compiles to the assembly code below. Line 1 increments the stack pointer to make some space to store the variable. Line 2 moves the value 10 into the register 0 (`r0`). Line 3 takes this value and uses the `StoreIdx` instruction to store the value of a register to a memory address, i.e. the space made on the stack. Finally, Line 4 cleans up the stack by decrementing the stack pointer, and Line 5 exits the program.  

```
0| .main:
1|     ADDI sp sp #1
2|     MOVI r0 #10
3|     ST r0 bp #0
4|     SUBI sp sp #1
5|     EXIT
```

# Conditionals
The language supports if and if-else statements to allow branched execution. For example:

```c
if True {
  print(1)
}
```

This compiles to the assembly below. Line 1 moves the value `1` into `r0`, i.e. storing the representation of `True` in `r0`. Line 2 checks whether the value stored in `r0` is false. If it is, the body of the if-statement will be skipped and execution will branch to the label `.0`, i.e. the end of the program. If the value stored in `r0` is True, then the branch will not be taken and execution will continue to lines 3 and 4 which print out the value of `r0`.

This assembly code could be optimised to remove the branch and simply exit, however, performing no optimisations made it easier to produce test programs for the [Processor Simulator](https://github.com/BakerSmithA/processor_sim). 

```
0| .main:
1|     MOVI r0 #1
2|     BF r0 .0
3|     MOVI r1 #1
4|     PRINT r1
5| .0:
6|     EXIT
```

# Loops
The language supports while loops and for loops (which are compiled to while loops). For example:

```c
def main() {
  for let i in 0..<10 {
    print(i)
  }
}
```

This compiles to the assembly below. 

```
00| .main:
01|     ADDI sp sp #1
02|     MOVI r0 #0
03|     ST r0 bp #0
04|     LD r0 bp #0
05|     MOVI r1 #10
06|     LT r0 r0 r1
07|     BF r0 .1
08| .0:
09|     LD r1 bp #0
10|     PRINT r1
11|     LD r1 bp #0
12|     MOVI r2 #1
13|     ADD r1 r1 r2
14|     ST r1 bp #0
15|     LD r0 bp #0
16|     MOVI r1 #10
17|     LT r0 r0 r1
18|     BT r0 .0
19| .1:
20|     SUBI sp sp #1
21|     EXIT
```

# Arrays

# Function Calls
