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
