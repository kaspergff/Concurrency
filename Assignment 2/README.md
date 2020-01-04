# INFOB3CC Assignment 2: Netchange

http://www.cs.uu.nl/docs/vakken/b3cc/assignments.html

Group 113

Kasper de Graaff        6281427
Stefan van der Pijl		6201202

This is our implementation of the net change algorithm for assignment 2 of concurrency.

A few important remarks.
- During the testing with Tomjudge we came to different results. Sometimes the tests went well, other times the result was less positive.

- In our implementation we used the value 25 for N (network-sized). This is an arbitrary number that stands for infinite. We chose it for this assignment because the test networks can never reach this number.

We have tried to use STM as much as possible. In some places we have had to implement a lock. This was of course necessary when writing to an output and a lock was used with the fail and repair functions.

