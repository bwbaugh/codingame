This contest is a 2-week optimization challenge. Please note that the available 2 weeks does not reflect the difficulty of the challenge. Also it does not mean that you will have to code all day 2 weeks in a row.

It is in not necessary to succeed all of the given test cases to have a good score. However, solutions are ranked first by percentage, then by score. To rank your solution, you must submit it. We have made it possible for you to submit a solution multiple times across the next 2 weeks, so that you may attempt to improve your code at any moment.

The players with the most points across all validation test cases at the end of the 2 weeks will be the winners.

     The Goal


Take out the armed forces in search of incriminating data. The more data gets saved, the higher you score. Other aspects of your performance will also affect your score.
     Rules

The game is played in a zone 16000 units wide by 9000 units high.
You control a man named Wolff. Wolff must defend a given number of data points from a given number of enemies that are placed across the game zone.

Wolff works as follows:
Wolff can be told to move to any point within the game zone by outputting the MOVE command followed by coordinates X Y. The top‑left point is 0 0.
Each turn, Wolff will move exactly 1000 units towards the target coordinates, or onto the target coordinates if he is less than 1000 units away.
Output the SHOOT command followed by the id of an enemy and Wolff will shoot that enemy, dealing him damage inversely proportional to the distance between them. The exact value is available in the Expert rules section.
Attempting to shoot an enemy who is already dead or does not exist will cause a game over. You score zero points.
If Wolff comes within 2000 units of any enemy, he is killed by the enemy and you lose the game. You score zero points.

Data points work as follows:
Data points are placed at the start and cannot be moved.
Data points are worth 100 points apiece as long as they stay in play. If an enemy arrives at a data point, the data is lost immediately as well as the associated points.
If all data points are collected by the enemies, the game ends.

Enemies work as follows:
Each turn, every enemy will target the closest data point and step 500 units towards it. If the enemy is less than 500 units away, the data point is lost and the enemy moves onto its coordinates.
Two enemies may occupy the same coordinates.
Each enemy starts with a given number of life points. Dealing enough damage to the lower the enemy's life points to zero kills the enemy and you score 10 points.

The order in which actions happens in between two rounds is:
Enemies move towards their targets.
If a MOVE command was given, Wolff moves towards his target.
Game over if an enemy is close enough to Wolff.
If a SHOOT command was given, Wolff shoots an enemy.
Enemies with zero life points are removed from play.
Enemies collect data points they share coordinates with.

When there are no more enemies or data points left, the game ends and you will score extra points according to the number of shots fired (in addition to the 100 points per data point and 10 points per kill).
This bonus is calculated with the following formula: DP * max(0, (L - 3*S)) * 3
Where:
DP is the number of data points left.
L is the total amount of life points enemies have among themselves at the start of the game.
S is the total number of shots fired during play.
 
Victory Conditions
You score at least 1 point.
 
Lose Conditions
You are caught by an enemy or you score 0 points.
Your program times out or you provide invalid output.
     Expert Rules

The coordinate system of the game uses whole numbers only. If Wolff or an enemy should land on non-whole coordinates, those coordinates are rounded down.

For example, if an enemy were to move from X=0, Y=0 towards X=500, Y=500, since they may only travel 500 units in one turn they should land on X=353.553, Y=353.553 but will in fact land on X=353, Y=353.

When shooting an enemy, the damage dealt is 125 000 divided by x1.2, rounded to the nearest integer ; x is equal to the Euclidean distance between Wolff and the enemy.

If several data points tie as the closest to an enemy, that enemy will aim for the data point with the smallest id.
     Note

Don’t forget to run the tests by launching them from the “Test cases” window. You do not have to pass all tests to enter the leaderboard. Each test you pass will earn you some points.

Warning: the tests provided are similar to the validation tests used to compute the final score but remain different. This is a “hardcoding” prevention mechanism. Harcoded solutions will not get any points. In addition, new validation tests may be added during the contest.

Your score is computed from the total points earned across all test cases.

Do not hesitate to switch to debug mode () if you get stuck. In debug mode, hover over an enemy or datapoint to see their coordinates.
     Game Input

The program must, within an infinite loop, read the contextual data from the standard input (data and enemy positions) and provide to the standard output the desired instruction.
Input for one game turn
Line 1: two space-separated integers x and y, the coordinates of your character.

Line 2: one integer dataCount, the initial amount of data points.

Next dataCount lines : three space-separated integers dataId, dataX & dataY, the unique id and coordinates of a data point.

Next line: one integer enemyCount, the amount of enemies left to take out.

Next enemyCount lines: 4 space-separated integers enemyId, enemyX, enemyY & enemyLife, the unique id, current coordinates and life points of an enemy.

Output for one game turn
A single line: MOVE followed by two integers targetX and targetY, the coordinates you want your character to move towards. Or SHOOT followed by an enemy id to shoot that enemy. You may also append some text which will be displayed on screen e.g. MOVE 0 0 hello world.
Constraints
0 ≤ x < 16000

0 ≤ y < 9000

1 ≤ dataCount < 100

1 ≤ enemyCount < 100

1 ≤ enemyLife < 150

Response time per game turn ≤ 100ms
