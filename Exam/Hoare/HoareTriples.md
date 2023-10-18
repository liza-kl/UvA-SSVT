# How to show correctness of Hoare Triple 
- Exercise 7 in Workshop

> The correct method is to first calculate the weakest precondition from the
> assignment and the postcondition, and then check that this weakest precondition
> is weaker than the precondition mentioned in the Hoare triple.

Example:
Precondition: λn -> x = n²
S: λn -> n + 1 
Postcondition:  λn -> x = (n - 1)²



1.1. Which of the following are correct Hoare statements, state how you find out.
    - First, check for counterexample. One counter example means invalidity.
    - Always make a screenshot of the https://www.wolframalpha.com/ plot to show
    invalidity (yields extra points)

1.2. Discuss weakest preconditions

### Finding Strongest post condition
## Hoare Logic Rules 
