# Exercise 7 #

Is there a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R?
Starttime: 15:23
Endtime:   15:43

Answer: Yes, there is a difference.
Proof by Counterexample:
Hypothesis: There is no difference between the symmetric closure of the transitive
            closure of a relation R and the transitive closure of the symmetric closure of R

Example: R = `[(1,2),(2,3),(3,4)]`

Case 1: Symmetric closure of the transitive closure

Transitive Clusure first: `[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]`

Symmetric closure second: `[(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)]`

Case 2: Transitive closure of the Symmetric closure
Symmetric Closure first: `[(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]`
Transitive closure second: `[(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(1,1),(1,3),(2,2),(2,4),(3,1),(3,3),(4,2),(4,4),(1,4),(4,1)]`

Counterexample : `[(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)] =/= [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3),(1,1),(1,3),(2,2),(2,4),(3,1),(3,3),(4,2),(4,4),(1,4),(4,1)]`
