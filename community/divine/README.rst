######
divine
######


****
Goal
****

In a typical match-3 game, the goal is to create alignments of 3 or
more identical tokens by swapping two adjacent tokens.

Your program must implement a hint system for a match-3 game by listing
all pairs of tokens that could be swapped to generate horizontal or
vertical alignements of 3 or more tokens within a 2D grid of 9 rows by
9 columns.

The pairs must be given ordered by row, then colum, i.e. from top left
to bottom right.

A given pair must be given only once, i.e. if you report (A,B) you must
not report (B,A) as well.

For instance, if the token at row 5 and column 7 can be swapped within
each of its four neighbors, then the expected report is::

    4 7 5 7
    5 6 5 7
    5 7 5 8
    5 7 6 7


*****
Input
*****

9 lines: 9 integers, separated by spaces. Each value represents a given
kind of token.


******
Output
******

Line 1: N, the number of pairs

N next lines: one pair expressed as 4 space-separated integers
representing the row and column of swappable pairs, i.e. row1 col1
row2 col2.


*******
Example
*******

Input
=====

.. code-block::

    4 4 1 5 1 2 5 5 1
    1 4 2 1 1 2 2 4 5
    1 1 5 4 5 4 4 5 1
    3 2 1 3 1 3 3 1 2
    1 5 3 3 5 5 2 5 2
    1 1 5 5 3 5 2 3 1
    2 2 5 5 4 4 3 1 3
    1 4 4 3 4 1 5 5 4
    3 4 1 5 5 4 1 2 5

Output
======

.. code-block::

    26
    0 2 1 2
    0 3 1 3
    0 8 1 8
    1 7 1 8
    1 7 2 7
    2 0 3 0
    2 2 3 2
    2 3 2 4
    2 4 3 4
    3 0 4 0
    3 3 3 4
    4 1 4 2
    4 1 5 1
    4 3 4 4
    4 3 5 3
    4 4 5 4
    4 6 4 7
    5 3 5 4
    5 4 5 5
    5 7 6 7
    6 0 7 0
    7 3 7 4
    7 3 8 3
    7 4 7 5
    7 8 8 8
    8 4 8 5
