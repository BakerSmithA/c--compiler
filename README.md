# C-- Compiler
C-- is a programming language much like C, but with a few features missing allowing for easier compilation. The language still includes the essentials such as: variables, loops, functions with arguments and return values, arrays. Example programs can be found [here](https://github.com/BakerSmithA/c--compiler/tree/master/examples/benchmark). They can be run using the processor simulator [here](https://github.com/BakerSmithA/processor_sim).

# Install

Clone the repo, and then run the following commands. The compiled output can be found in `dist/build/c--`.

```
> cd c--compiler
> cabal sandbox init
> cabal install
> cabal build
```

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

This compiles to the assembly code below. Line 1 increments the stack pointer to make some space to store the variable. Line 2 moves the value 10 into the register 0 (`r0`). Line 3 takes this value and uses the `ST` instruction to store the value of a register to a memory address, i.e. the space made on the stack. Finally, Line 4 cleans up the stack by decrementing the stack pointer, and Line 5 exits the program.  

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

This compiles to the assembly below. Lines 1-3 setup space on the stack for the variable `i`, and populate the address with the value `0`, i.e. the initial value of `i`. Lines 4-6 check whether the terminsation condition of the loop is fulfilled, i.e. is `i` less than `10`. If the condition is fulfilled, then a branch to the function clean up (lines 20-21) is taken. If the loop is still being run, execution continues to line 9. Lines 9-10 perform the body of the loop, printing out the value of `i`. Lines 11-14 increment the value of `i`, such as is the semantics of a for-loop. Finally, lines 15-18 re-check the loop termination condition and chooses whether to execute the body of the loop again.

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
Arrays are used to store multiple values in a contigous memory region, and allow for access using the subscript operator. For example:

```c
def main() {
  let xs = [5, 10, 15]
  print(xs[1])
}
```

This compiles to the assembly below. Line 1 makes space on the stack for the three elements of the array, plus a pointer to the start of the array (i.e. the value of the variable `xs`). Lines 2-4 calculates the global offset of the start of the array and stores the value on the stack (in variable `xs`). By storing the global offset, it makes it easy to pass pointers to arrays between functions. Lines 5-7 store the value `5` at index `0` in the array. Lines 8-9 store the value `10` at index `1` in the array. Lines 10-11 store the value `15` at index `2` in the array. Lines 12-15 fetch the value at index `1` in the array, and print the value to the screen. Finally, Lines 16-17 clean up the stack and exit the program.

```
00| .main:
01|     ADDI sp sp #4
02|     MOV r0 sp
03|     SUBI r0 r0 #3
04|     ST r0 bp #0
05|     LD r0 bp #0
06|     MOVI r1 #5
07|     ST r1 r0 #0
08|     MOVI r1 #10
09|     ST r1 r0 #1
10|     MOVI r1 #15
11|     ST r1 r0 #2
12|     LD r0 bp #0
13|     MOVI r1 #1
14|     LD r0 r0 r1
15|     PRINT r0
16|     SUBI sp sp #4
17|     EXIT
```

## Strings
Strings are compiled to arrays of integers terminated with a `0`. The values stored at each position are the ASCII values of each character. The `printc` command can then be used to print out the ACII character corresponding to the integer value. For example:

```c
def main() {
  let s = "abc"
  printc(s[1])
}
```

This compiles to the assembly below. Lines 1-13 sets up the variable `s` and stores the ASCII values corresponding to `a`, `b`, and `c` in the array (i.e. 97, 98, and 99 respectively). The value `0` is also stored in the array, as strings are null terminated. Lines 14-17 print out the character corresponding to the value at index `1` in `s`. Finally, the stack is cleaned up and the function exited.

```
00| .main:
01|     ADDI sp sp #5
02|     MOV r0 sp
03|     SUBI r0 r0 #4
04|     ST r0 bp #0
05|     LD r0 bp #0
06|     MOVI r1 #97
07|     ST r1 r0 #0
08|     MOVI r1 #98
09|     ST r1 r0 #1
10|     MOVI r1 #99
11|     ST r1 r0 #2
12|     MOVI r1 #0
13|     ST r1 r0 #3
14|     LD r0 bp #0
15|     MOVI r1 #1
16|     LD r0 r0 r1
17|     PRINTC r0
18|     SUBI sp sp #5
19|     EXIT
```

# Function Calls

```c
def main() {
  let xs = [5, 10, 15]
  print(sum_arr(3, xs))
}

def sum_arr(n: Int, arr: Int[]) -> Int {
  let total = 0
  for let i in 0..<n {
    total = total + arr[i]
  }
  return total
}
```

This is compiled to the assembly below. Ommitted lines 1-11 setup the array `xs`. 

## Entry
Line 12 and 13 saves the value `3` (`n` argument to `sum_arr`) and a pointer to `xs` (`arr` argument to `sum_arr`). These will be pushed onto the stack later, however, they are calculated now because the base pointer of the stack frame is modified. Lines 14-15 save the base pointer and stack pointer so they can be restored after the function invocation. Line 16 increments the stack pointer to account for the SP and BP being pushed to the stack. Line 17 updates the base pointer to the new base of call stack. Line 18 saves the address of the label `.0` in the Link Register. This will be the location returned to when the `RET` instruction is executed by the `sum_arr` function. Like the x86 calling convention, the callee places arguments on the stack for the called function to use. Lines 19-20 push the number `3` and a pointer to `xs` onto the stack, as arguments to the `sum_arr` function. Since the base pointer was updated (line 17), the callee can refer to these variables by offsetting from the base pointer. 

## Call
Line 22 branches to the label `.sum_arr`, which sets the PC to be at line 32. Omitted lines perform the body of the function, and store the value of `total` in a variable at address `bp + 3`. Line 58 loads the value of `total` and stores it in the `ret` register. This is a special register for storing a return value. Line 59 cleans up the stack. Line 60 then branches back to the address stored in the Link Register. 

## Clean-Up
Line 24 pops the arguments to `sum_arr` from the stack. Lines 25-26 then restore the old values of the BP and SP, and line 27 pops them from the stack too. Lines 28-29 print the value returned by `sum_arr`, and then lines 30-31 clean up the stack and exit the program.

```
000| .main:
     ...
012|     MOVI r1 #3
013|     LD r2 bp #0
014|     ST bp sp #0
015|     ST lr sp #1
016|     ADDI sp sp #2
017|     MOV bp sp
018|     MOVI lr .0
019|     ST r1 sp #0
020|     ST r2 sp #1
021|     ADDI sp sp #2
022|     B .sum_arr
023| .0:
024|     SUBI sp sp #2
025|     LD lr sp #-1
026|     LD bp sp #-2
027|     SUBI sp sp #2
028|     MOV r0 ret
029|     PRINT r0
030|     SUBI sp sp #4
031|     EXIT

032| .sum_arr:
033|     ADDI sp sp #2
     ...
058|     LD ret bp #3
059|     SUBI sp sp #2
060|     RET
```
