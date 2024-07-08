from enum import Enum


class WeightResult(Enum):
    """
    Represents the possible outcome results of a weighing in the game.
    """

    GT = '>'
    LT = '<'
    EQ = '='
