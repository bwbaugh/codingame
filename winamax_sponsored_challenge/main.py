"""Determine the winner of a game of war.

.. note::

    Unfortunately, a lot of functions in this module employ the bad
    practice of both performing a side effect (like mutating the input)
    and returning a value. If this was more than a "just for fun puzzle
    for the weekend", then it'd be worth refactoring so that a function
    only returns a value _or_ performs a side effect.
"""
import collections
import functools
import logging
import sys


#: How many cards to set aside when a war occurs.
NUM_WAR_CARDS = 3

logging.basicConfig(stream=sys.stderr, level=logging.INFO)
log = logging.getLogger(__name__)


class OutOfCardsError(Exception):
    pass


@functools.total_ordering
class CardOrderingMixin(object):

    """Provides comparison capabilities for :class:`Card`.

    Needs to be a separate class to work with :func:`total_ordering`.

    More info: <http://stackoverflow.com/a/12614638/1988505>
    """

    def __eq__(self, other):
        return bool(self.value == other.value)

    def __lt__(self, other):
        return bool(
            Card.VALUE_POINT_MAP[self.value] <
            Card.VALUE_POINT_MAP[other.value]
        )


class Card(
    CardOrderingMixin,
    collections.namedtuple(typename='Card', field_names='value suite'),
):

    #: Ordered list of values from weakest to strongest.
    VALUE_ORDER = '2 3 4 5 6 7 8 9 10 J Q K A'.split()
    #: Set of valid values.
    VALUE_SET = frozenset(VALUE_ORDER)
    #: Set of valid suites.
    SUITE_SET = frozenset('D H C S'.split())

    #: Mapping of card value to integer number of points where
    #: stronger is larger.
    # NOTE: Could also use the `bisect` library to build an `index`
    #   function that could operate just on the `VALUE_ORDER` list.
    VALUE_POINT_MAP = {
        value: point
        for value, point in zip(VALUE_ORDER, range(len(VALUE_ORDER)))
    }

    @classmethod
    def loads(cls, input_string):
        # The last character is the suite, everything else the value.
        instance = cls(value=input_string[:-1], suite=input_string[-1])
        # Validation is probably overkill for a program like this. If
        #   performance was a consideration, we could ask if we can
        #   make the assumption that validation was already performed.
        if instance.suite not in Card.SUITE_SET:
            raise ValueError('Unknown suite: %s' % instance.suite)
        if instance.value not in Card.VALUE_SET:
            raise ValueError('Unknown value: %s' % instance.value)
        return instance


def main():
    # I've seen games of war with more than two players.
    cards_map = parse_initial_cards(player_list=[1, 2])
    log.debug('Parsed cards_map: %r', cards_map)
    num_rounds = 0
    try:
        while all(cards for cards in cards_map.itervalues()):
            num_rounds += 1
            log.info('Starting round %s.', num_rounds)
            for each_player in sorted(cards_map):
                log.debug(
                    'Player %s has %s cards.',
                    each_player,
                    len(cards_map[each_player]),
                )
            round_data = play_round(cards_map=cards_map)
            # If performance is an issue when inserting, consider using
            #   `collections.deque` for the `appendleft()` method.
            cards_map[round_data['winner']] = (
                round_data['winner_cards'] + cards_map[round_data['winner']]
            )
    except OutOfCardsError:
        log.info('Game is a draw after %s rounds.', num_rounds)
        print 'PAT'
    else:
        winner = determine_final_winner(cards_map=cards_map)
        log.info(
            'Final winner is player %s after %s rounds.',
            winner,
            num_rounds,
        )
        print '{winner} {num_rounds}'.format(
            winner=winner,
            num_rounds=num_rounds,
        )


def parse_initial_cards(player_list):
    """Reads the cards from stdin."""
    cards_map = {}
    for player_num in player_list:
        num_cards = int(raw_input())
        cards_map[player_num] = [
            Card.loads(input_string=raw_input()) for __ in xrange(num_cards)
        ]
        # The last card is the top of the deck.
        cards_map[player_num].reverse()
    return cards_map


def play_round(cards_map):
    """Plays a single round, mutating the `cards_map` input.

    :raises OutOfCardsError: If a player runs out of cards during a war.
    """
    picked_cards_map = pop_player_cards(cards_map=cards_map)
    if is_war(picked_cards_map=picked_cards_map):
        log.info('War starting.')
        war_data = play_war(cards_map=cards_map)
        log.info('War finished.')
        round_winner = war_data['winner']
    else:
        round_winner = determine_round_winner(
            picked_cards_map=picked_cards_map,
        )
    winner_cards = order_winner_cards(picked_cards_map=picked_cards_map)
    log.debug('round_winner: %s', round_winner)
    log.debug('winner_cards: %s', winner_cards)
    return {
        'winner': round_winner,
        'winner_cards': winner_cards,
    }


def pop_player_cards(cards_map):
    """Removes and returns the next card for each player.

    :raises OutOfCardsError: If a player has no cards left.
    """
    picked_cards_map = {}
    for each_player, each_cards in cards_map.iteritems():
        try:
            picked_cards_map[each_player] = each_cards.pop()
        except IndexError:
            raise OutOfCardsError
    log.debug('picked_cards_map: %r', picked_cards_map)
    return picked_cards_map


def is_war(picked_cards_map):
    """There is a war if any picked cards are equal in value."""
    seen_values = set()
    for each_card in picked_cards_map.itervalues():
        # XXX: This assumes that each card value has a unique strength.
        if each_card.value in seen_values:
            return True
        seen_values.add(each_card.value)
    return False


def play_war(cards_map):
    """
    :raises OutOfCardsError: If a player runs out of cards.
    """
    war_cards_map = {}
    for each_player, each_cards in cards_map.iteritems():
        cards = []
        for __ in xrange(NUM_WAR_CARDS):
            try:
                cards.append(each_cards.pop())
            except IndexError:
                raise OutOfCardsError
        war_cards_map[each_player] = cards
    log.debug('war_cards_map: %s', war_cards_map)

    round_data = play_round(cards_map=cards_map)
    # XXX: I'm not very sure about the order of operations after a war.
    #   The problem statement is a little ambiguous on exactly how and
    #   when to put the cards from the battle(s) and war(s) into the
    #   player's deck.
    cards_map[round_data['winner']] = (
        order_war_winner_cards(war_cards_map=war_cards_map) +
        cards_map[round_data['winner']]
    )
    cards_map[round_data['winner']] = (
        round_data['winner_cards'] + cards_map[round_data['winner']]
    )

    return {'winner': round_data['winner']}


def determine_round_winner(picked_cards_map):
    """Returns the player with the strongest card.

    .. note::

        Verify that there isn't a war before calling this function.
    """
    return max(picked_cards_map, key=picked_cards_map.get)


def order_winner_cards(picked_cards_map):
    """First the cards from the first player, then the one from the second."""
    return [
        picked_cards_map[each_player]
        for each_player in sorted(picked_cards_map)
    ]


def order_war_winner_cards(war_cards_map):
    """First the cards from the first player, then the one from the second."""
    cards = []
    for each_player in sorted(war_cards_map):
        cards.extend(war_cards_map[each_player])
    return cards


def determine_final_winner(cards_map):
    """Picks the winner with the largest amount of cards remaining."""
    return sorted(
        cards_map,
        key=lambda player: len(cards_map[player]),
        reverse=True,
    )[0]


if __name__ == '__main__':
    main()
