# Erlang Scratchpad

Playing around with Erlang.

## Prerequisites

A regular Erlang/OTP installation will do. Tested on Erlang/OTP version 22.

## Getting Started

The code is meant to run in the Eshell for experimentation.

Open a terminal and start the Erlang interactive shell (Eshell) by typing:

```
erl
```

Next, import one of the modules in this project. In the Eshell this can be done with the 'c(module_name)' command. Import the 'maths' module by typing:

```
c(maths)
```

The shell should return '{:ok, maths}' if everything went as expected. The maths module is now available to you for use. Check the '-export' module attribute for all the functions that are available in a given module and their corresponding arity.

Let's use the multiply function. With the 'maths' module available to us, type the following code:

```
maths:multiply(5,6).
```

The resulting output should be 30.
