####################
Ultimate Tic-Tac-Toe
####################

.. note::

    This is a league based challenge.

    For this challenge, multiple versions of the same game are
    available. Once you have proven your skills on this first version,
    you will access a higher league and extra rules will be unlocked.


********
The Goal
********

Tic-tac-toe is a turn-based game, where the objective is to get three
in a row.


*****
Rules
*****

The game is played on a 3x3 grid. You must output the coordinate of the
cell you want to mark. The first player to get 3 of their mark in a row
(vertically, horizontally or diagonally) wins.

You can download the source code of this game on `GitHub
<https://github.com/CodinGame/game-ultimate-tictactoe>`_. You can
create your own game using the `CodinGame SDK
<https://github.com/CodinGame/codingame-sdk-doc>`_!


**********
Game Input
**********

Input for one game turn
=======================

Line 1: 2 space separated integers ``opponentRow`` and ``opponentCol``,
the opponent's last action (``-1 -1`` for the first turn). 

Line 2: the number of valid actions for this turn,
``validActionCount``.

Next ``validActionCount`` lines: 2 space separated integers ``row`` and
``col``, the coordinates you're allowed to play at.

Output for one game turn
========================

Line 1: 2 space separated integers ``row`` and ``col``.

Constraints
===========

Response time for first turn ≤ 1000ms
Response time for one turn ≤ 100ms

.. note::

    What is in store for me in the higher leagues?

    The extra rules available in higher leagues are:
    Next game will be played on 9 tic-tac-toe boards!
