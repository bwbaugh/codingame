from __future__ import print_function

import collections
import math
import sys


PLAYER_SPEED = 1000
ZOMBIE_SPEED = 400

WEAPON_RANGE = 2000

Point = collections.namedtuple('Point', 'x y')


def main():
    while 1:
        player = _get_player()
        humans = _get_humans()
        zombies = _get_zombies()
        next_point = pick_next_point(player, humans, zombies)
        print('{point.x} {point.y}'.format(point=next_point))


def _get_player():
    x, y = [int(x) for x in input().split()]
    return Point(x=x, y=y)


def _get_humans():
    humans = {}
    human_count = int(input())
    for _ in range(human_count):
        human_id, human_x, human_y = [int(x) for x in input().split()]
        humans[human_id] = Point(x=human_x, y=human_y)
    return humans


def _get_zombies():
    zombies = {'current': {}, 'future': {}}
    zombie_count = int(input())
    for i in range(zombie_count):
        zombie_id, zombie_x, zombie_y, zombie_xnext, zombie_ynext = [
            int(j) for j in input().split()
        ]
        zombies['current'][zombie_id] = Point(x=zombie_x, y=zombie_y)
        zombies['future'][zombie_id] = Point(x=zombie_xnext, y=zombie_ynext)
    return zombies


def pick_next_point(player, humans, zombies):
    human_distance_map = calc_distances(target=player, mapping=humans)
    human_zombie_map = {
        each_id: calc_distances(target=each_point, mapping=zombies['future'])
        for each_id, each_point in humans.items()
    }
    doomed_humans = find_doomed_humans(
        human_distance_map=human_distance_map,
        human_zombie_map=human_zombie_map,
    )
    for each_id in doomed_humans:
        del human_distance_map[each_id]
    sorted_human_distance = sorted(
        human_distance_map,
        key=human_distance_map.get,
    )
    nearest_human = sorted_human_distance[0]
    return humans[nearest_human]


def calc_distances(target, mapping):
    return {
        each_id: distance(a=target, b=each_point)
        for (each_id, each_point) in mapping.items()
    }


def distance(a, b):
    return math.sqrt((b.x - a.x) ** 2 + (b.y - a.y) ** 2)


def find_doomed_humans(human_distance_map, human_zombie_map):
    doomed_humans = []
    for human_id, human_distance in human_distance_map.items():
        sorted_zombie_distance = sorted(
            human_zombie_map[human_id],
            key=human_zombie_map[human_id].get,
        )
        nearest_zombie = sorted_zombie_distance[0]
        zombie_distance = human_zombie_map[human_id][nearest_zombie]
        zombie_turns = turns_for_zombie(distance=zombie_distance)
        player_distance = human_distance_map[human_id] - WEAPON_RANGE
        player_turns = turns_for_player(distance=player_distance)
        print('human_id', human_id, file=sys.stderr)
        print('nearest_zombie', nearest_zombie, file=sys.stderr)
        print('zombie_distance', zombie_distance, file=sys.stderr)
        print('zombie_turns', zombie_turns, file=sys.stderr)
        print('player_distance', player_distance, file=sys.stderr)
        print('player_turns', player_turns, file=sys.stderr)
        # XXX: `- 1` is a fudge factor to account for the player
        #   probably being able to shoot the zombie on the way to the
        #   human.
        if zombie_turns < (player_turns - 1):
            doomed_humans.append(human_id)
        print('', file=sys.stderr)
    return doomed_humans


def turns_for_player(distance):
    return _turns(distance=distance, speed=PLAYER_SPEED)


def turns_for_zombie(distance):
    return _turns(distance=distance, speed=ZOMBIE_SPEED)


def _turns(distance, speed):
    return int(math.ceil(distance / speed))


if __name__ == '__main__':
    main()
