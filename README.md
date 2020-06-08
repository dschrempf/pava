# Pool adjacent violators algorithm

Compute greatest convex majorants and least concave minorants using the pool
adjacent violators algorithm.

The pool adjacent violators algorithm (PAVA) is an iterative algorithm for
solving monotonic regression problems. In particular, (antitonic) regression is
the computation of a non-decreasing (non-increasing) sequence of values such
that a given problem is optimized. PAVA can also be used to compute the greatest
convex minorant and the least concave majorant of a given set of observables.

At the moment, greatest convex majorants and least concave minorants can be
computed efficiently. More general isotonic regression is not yet supported, but
may be in future releases.

Reading list:
- A [good introduction to PAVA](https://repository.tudelft.nl/islandora/object/uuid:5a111157-1a92-4176-9c8e-0b848feb7c30?collection=education).
- [Isotone regression in R](https://cran.r-project.org/web/packages/isotone/index.html).
- [Computation of greatest convex minorants in R](http://search.r-project.org/library/fdrtool/html/gcmlcm.html).
- [Wikipedia article on isotonic regression](https://en.wikipedia.org/wiki/Isotonic_regression).
