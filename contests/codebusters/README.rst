===========
CodeBusters
===========


********
The Goal
********

Capture more ghosts than the rival team.


*****
Rules
*****

The game is played on a map 16001 units wide and 9001 units high. The
coordinate X=0, Y=0 is the top left pixel.

Each player controls a team of several busters. The teams start out at
opposite corners of the map, near their base. Ghosts are scattered
across the map, and need to be trapped and brought back to a player's
base. Each ghost inside the base or trapped by a buster is worth a
point for the owner. However, you may lose a point if one of your
busters releases a ghost outside of your base.

The map works as follows:

-   There are always 2 teams in play.
-   At the start of the game, each player is given a team id. This
    indicates on which corner their base is positioned. The top left
    corner (X=0, Y=0) is for team 0. The bottom right corner ( X=16000,
    Y=9000) is for team 1.
-   Fog makes it impossible to know the positions of ghosts and rival
    busters unless they are within 2200 units from one of your own
    busters.
-   Each buster has a unique id. Each ghost has a unique id. Ghosts and
    busters with the same id are not correlated.

Busters work as follows:

-   Every turn, a buster may do one of the following actions: ``MOVE``,
    ``BUST`` or ``RELEASE``
-   ``MOVE`` followed by map coordinates will make the buster advance
     800 units towards that point. The position will be rounded to the
     nearest integer.
-   ``BUST`` followed by a ghost's id will cause the buster to suck
    that ghost into his ghost trap if he is within a range of 1760
    units around the target ghost but not closer than 900 units.
    Trapped ghosts are no longer visible on the map.
-   A buster may carry no more than 1 ghost at a time.
-   ``RELEASE`` will cause the buster to drop the ghost he is carrying
    back onto the map. If this is done within 1600 units of a corner of
    the map on which a base is positioned, the ghost is removed from
    play and secures a point for the base's owner.

Ghosts work as follows:

-   Ghosts remain stationary unless busters are within 2200 units of
    them and they are not in the process of being trapped. In this
    case, they will move 400 units away from the closest buster. In
    case of a tie, it'll be the mean position of the busters.
-   If several busters attempt to trap a ghost, the team with the most
    busters will take priority. Within that team, the closest buster
    will take the ghost. If both teams have an equal number of busters
    attempting to trap the ghost, it will not be trapped on that turn.
-   A ghost being carried by a buster will escape if that buster
    attempts to trap a different ghost.
-   The game ends once all ghosts have been captured or after the time
    limit of 400 turns.

The game state of every turn is given to you as a list of entities,
each with an id, position, type, state and value.

The type will be:

-   0 for a buster from team 0.
-   1 for a buster from team 1.
-   -1 for a ghost.

The state will be:

-   For busters:
    -   0: buster not carrying any ghost.
    -   1: buster carrying a ghost.
-   For ghosts, it is always 0.

The value will be:

-   For busters, if the buster is carrying a ghost, that ghost's id,
    otherwise -1.
-   For ghosts, it is 0 unless several busters tied in trying to trap
    it, in which case it is equal to the amount of busters that
    attempted  to trap it on that turn.

Victory Conditions
==================

Have captured more ghosts than the rival team at the end of the game.

Lose Conditions
===============

-   Your program provides unrecognized output.
-   Your program times out.
-   You have less ghosts than the opponent at the end of the game.

.. note::

    The program must first read initialization input and then, within
    an infinite loop, read the contextual data from the standard input
    and provide to the standard output the desired instructions.


**********
Game Input
**********

Initialization input
====================

-   Line 1: an integer bustersPerPlayer for the amount of busters each
    team controls.
-   Line 2: an integer ghostCount for the amount of ghosts available on
    the map.
-   Line 3: an integer myTeamId for the team you are playing as.

Input for one game turn
=======================

-   Line 1: an integer entities for the amount of entities on the map
    that are visible to you.
-   Next entities lines: 6 space separated integers entityId, x, y,
    entityType, state & value. Represents a buster or a ghost.

Output for one game turn
========================

One line for each buster: one of the following:

-   ``MOVE`` followed by two integers ``x`` and ``y``
-   ``BUST`` followed by one integer ``ghostId``
-   ``RELEASE``

You may append text to your instructions, it will be displayed in the viewer.

Constraints
===========

2 ≤ ``bustersPerPlayer`` ≤ 5
8 ≤ ``ghostCount`` ≤ 28

Response time per turn ≤ 100ms


***********************************************
What is in store for me in the higher leagues ?
***********************************************

The extra rules available in higher leagues are:

-   The ability to stun an opponent, causing any ghost he is carrying
    to escape.
-   Ghosts with endurance, who take several turns to successfully trap.
