import collections


Point = collections.namedtuple('Point', 'x y')


def main():
    while 1:
        player = [int(x) for x in input().split()]
        humans = _get_humans()
        zombies = _get_zombies()
        next_point = pick_next_point(player, humans, zombies)
        print('{point.x} {point.y}'.format(point=next_point))


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
    return next(iter(humans.values()))


if __name__ == '__main__':
    main()
