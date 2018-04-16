Futures in the compiler
=======================

Futures -- values that represent other values that may have not been computed yet -- are used throughout the compiler for many reasons.

This allows binding to be paused when an expression cannot be computed, and to
continue somewhere else.