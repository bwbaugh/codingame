###########################
Game of Drones - Multi game
###########################


********
The Goal
********

The goal of your mission is to control the Krysal zones. You direct
from 3 to 11 drones each game and you are confronted with 1, 2 or 3
other players, each managing as many drones as you. These drones are
arranged randomly (but fairly) on the map at the start of the game.

During the game there are always more zones to control than there are
players. The idea of the mission is simple enough: at the end of a
round (that is, when all players have sent their drones an order to
direct themselves to a point on the map), each zone that you control by
means of your drones brings you 1 point. At the end of the game (200
rounds), the player with the most points wins the game.


*****
Rules
*****

Let us continue to serious matters: how to control a zone!

One or several of your drones must fly over the sought-after zone in a
radius of **100** units around its center.

- If the zone is not controlled by another player and you are the only
  one to enter it during a round, you take control of it.
- If the zone is already controlled by another player, then you must
  have numeric superiority in the zone in order to take control of it.

You do not need to stay in a zone that you control to retain it if
there are no enemies inside!

The ground flown over by the drones is rectangular, it is **4,000**
units in wide and **1,800** units high. The coordinate 0,0 is at the
top-left.
For each round, you receive on the standard input of your program the
current position of all drones (both yours and those of enemies) and
you must write to the standard output a point of destination to be
reached by each of your drones. Note that each drone can travel a
maximum of **100** units of distance during a round; several rounds may
thus be necessary to reach a point!

The zones never overlap and there is no collision between drones
(several drones can occupy the same position).


**********
Game Input
**********

Your program must first read the initialization data from standard
input, then, in an infinite loop, read the contextual data of the game
(control of zones and positions of drones) and write to standard output
the actions for your drones. The protocol is detailed below. Your
program has 100ms maximum each round to send all instructions to your
drones.

Input for the initialization phase
==================================

:Line 1: 4 integers ``P`` ``ID`` ``D`` ``Z`` separated by a space.

``P``: number of players in the game (2 to 4 players),
``ID``: id of your player (0, 1, 2, or 3),
``D``: Number of drones in each team (3 to 11),
``Z``: Number of zones on the map (4 to 8).

:Z Following lines: the position X Y of the center of a zone (integers,
separated by a space). A zone is a circle with a radius of 100 units.

Input for one game turn
=======================

Z lines (1 line per zone): 1 integer corresponding to the id of the
team controlling the zone (0, 1, 2, or 3) or -1 if it is not
controlled. The zones are given in the same order as in the
initialization.
P*D following lines (1 line per drone): The first D lines contain the
coordinates of drones of a player with the id 0, the following D lines
those of the drones of player 1, and thus it continues until the last
player. Each line contains 2 integers (X Y) representing the
coordinates of a drone.


Output for one game turn
========================

D lines: each line corresponds to the coordinates X Y of a point of
destination to be reached by one of your drones. The first line
corresponds to the first of your drones that you were provided as
input, the next to the second, etc.


********
Synopsis
********

March 15th, 2020 : Violent earthquakes caused by the increased
discontinuity of Moho strike the desert of Kalahari.
A team of Geologists who left to explore the phenomenon and its
consequences discover that the faults have caused a mineral still
unknown to man to rise to the surface – Krysal, a mineral with
unparalleled regenerative powers…
The news quickly travels around the world, myriad looters attempt a
venture to the location, but the force of the earthquakes and the
hostility of the area make land exploration impossible.

You are part of a top-secret aerial exploration program and are at the
head of a team of drones capable of exploring and protecting high-risk
areas.
The only problem: having just arrived, we inform you that enemy troupes
wishing to take possession of the zone are on route…
