## In general mutation testing, what kind of mutants could be considered ‘relevant’?


## Consider commit-aware mutation testing. What properties should ’relevant’ mutants have in context?
https://link-springer-com.proxy.uba.uva.nl/article/10.1007/s10664-022-10138-1#Sec7 Section 3.1

A mutant is relevant if it impacts the behaviour of the committed code and the committed code impacts the behaviour of the mutant.

To identify such mutants we check, for each mutant, whether there is at least one test case that can make observable any behavioural difference between the mutant and:

1.
the program version that includes only the mutant (mutant in the pre-commit version).

2.
the program version that includes only the committed changes (post-commit version).

- Must be a reachable node in the control flow graph
## Mutation testing and test coverage
-- Which types of coverage do we have? How do they differ?

## What are flaws to the mutation testing approach?

## In general, in mutation testing, what kind of mutants could be considered ‘invalid’?

## When should we use mutation coverage and when should we use code coverage as measures of
test suite effectiveness?