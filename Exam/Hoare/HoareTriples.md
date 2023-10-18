## What is a a correct Hoare triple?
- Exercise 7 in Workshop

> The correct method is to first calculate the weakest precondition from the
> assignment and the postcondition, and then check that this weakest precondition
> is weaker than the precondition mentioned in the Hoare triple.

### Rules regarding correctness

1. If p′ is stronger that p, does it follow that {p′} f {q} still holds? 
-> Yes, strengthening the precondition **preserves** truth of the Hoare assertion.
2. If p′is weaker that p, does it follow that {p′} f {q} still holds? 
-> **No**. This step **broadens the range of possible tests for the function**, and some of
these tests may not be appropriate
3. If q′is stronger that q, does it follow that {p} f {q′} still holds?
-> **No**. This step strengthens the requirement on the output of the function, and tests
on f that are in accordance with the new specification may be too severe
4. If q′is weaker that q, does it follow that {p} f {q′} still holds? 
**Yes**, weakening the postcondition preserves truth of the Hoare assertion.

1.1. Which of the following are correct Hoare statements, state how you find out.
    - First, check for counterexample. One counter example means invalidity.
    - Always make a screenshot of the https://www.wolframalpha.com/ plot to show
    invalidity (yields extra points)



## Finding Weakest Precondition (Ex. 6 in Workshop)
1. Find the **input** domain for a function so that the output domain (the postcondition essentially)
is given. 
2. You substitute the "x" in the postcondition with the function definition. Then you need to simplify it, and this is
the weakest precondition.
3. Keep in mind, when giving the precondition to resolve after the same variable as the postcondition.

Examples: 
1.) {p} = ? // precondition
    f = \x -> 2x + 4  // function
    {q} \x -> 0 <= x < 8 // postcondition

Substitution: x = 2x + 4
Inserting in {q} and simplifying: 0 <= (2x + 4) < 8 -> if two of the <= are appearing,
you need to calculate both sides; so: 
2x + 4 = 8 | -4 ; / 2
x = 2 (upper bound)

0 <= 2x + 4 | -4; / 2
-2 <= x (lower bound)
=> -2 <= x < 2 is the weakest precondition which satisfies the postcondition. 

2.) {p} = ? // precondition
    f = \x -> x + 1
    {q} = \x -> 2x - 1 = A

- Substitution in {q} =>  2 (x + 1) - 1 = A
                    2x + 1 = A => Weakest precondition 

3.) 


### Finding Strongest post condition

- We need to substitue the inverse of f in the precondition. 
## Hoare Logic Rules 

## Importance of Precondition Weakening for Testing
- If you strengthen the requirements on your input, your testing procedure gets weaker
- Reason: the set of relevant tests gets smaller.
- Remember: the precondition specifies the relevant tests.

### Should preconditions be as weak as possible?

Source: https://stackoverflow.com/questions/70647427/weak-precondition-and-strong-postcondition-problems

"Both a weak precondition and a strong postcondition make a specification stronger.
A too-strong specification reduces flexibility for the implementer of a specification (an interface).
This results in a more complex and more expensive implementation. Furthermore,
not all clients might even need the strength, so the complexity and expense might go to waste"

## Importance of Postcondition Strengthening for Testing
If you weaken the requirements on your output, your testing procedure gets weaker
Reason: the requirements that you use for verifying the output get less specific.
Remember: the postcondition specifies the expected behaviour.

### Should postconditions be as strong as possible? Motivate your answer.

## Case of (2022^x)

Multiplying an even number by itself: When you raise an even number (e.g., 2, 4, 6, etc.) to any positive integer power, the result will always be even. For example, 2^2 = 4, 2^4 = 16, 2^6 = 64, and so on. In each case, the result is an even number.

Integer properties: Even integers are closed under multiplication. This means that when you multiply two even numbers or an even number by any integer, the result will also be even. So, when you raise an even number to any positive integer power, you're essentially multiplying that even number by itself multiple times, which keeps the result even.

In summary, even exponents always result in even numbers because even numbers multiplied by themselves, or by any other integer, will maintain their even nature. This is a fundamental property of even integers