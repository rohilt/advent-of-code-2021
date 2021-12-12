# Advent of Code 2021 Solutions (Haskell)
These are my solutions to the Advent of Code for 2021, written in Haskell.

## Software dependencies
Any recent version of `ghc` (the Haskell compiler) and `bash` should work to run this code.
I used `ghc` version 8.6.5 and `bash` version 5.0.17.

## Running the code
After the session cookie is stored in the `.session` file (see below), you can run the code by running the `run` Bash script.
This script requires one parameter, the day to run the code for.
It automatically retrieves the input file if it is not already present, and it also warns if the input file is not yet available.
It then runs the code for that specific problem, prints the output, and records how long it takes using the `time` command.

### Advent of Code session cookie
Create a text file `.session` that contains your session cookie for Advent of Code.
It should contain the following:
```
session=0123456789
```
`0123456789` should be replaced with your specific session cookie.
Also note that there should be no trailing newline at the end of this file.
