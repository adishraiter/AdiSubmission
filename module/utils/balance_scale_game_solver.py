from module.pages.balance_scale_game_pages import GamePage
from module.utils.balance_scale_weigh_result import WeightResult


class GameSolver:
    """
    Provides static methods to perform various actions required to solve the game.
    """

    @staticmethod
    def get_score(weighing: str) -> WeightResult:
        """
        Determine the result of the last weighing based on the provided weighing string.
        :param weighing: A string representing the data and the result of the last weighing.
        :return: The result of the last weighing as a WeightResult.
        """

        if WeightResult.EQ.value in weighing:
            return WeightResult.EQ
        elif WeightResult.GT.value in weighing:
            return WeightResult.GT
        else:
            return WeightResult.LT

    @staticmethod
    def fill_and_compare_tables(page: GamePage, digits: list) -> tuple:
        """
        - If the length of the bar numbers is odd, removes the last digit from the list and saves it.
        - Splits the digits between the left and right tables.
        - Resets the tables.
        - Fills the left and right tables with digits.
        - Performs a weighing.

        :param page: The GamePage instance used to interact with the game page.
        :param digits: The list of digits to be divided and inserted into the tables.
        :return: A tuple containing the left list, right list, and the remainder digit.
        """

        remainder = None if len(digits) % 2 == 0 else digits.pop()
        left_list = digits[:len(digits) // 2]
        right_list = digits[len(digits) // 2:]

        page.click_reset_button()
        page.enter_digit_to_table("left", left_list)
        page.enter_digit_to_table("right", right_list)
        page.click_weight_button()

        return left_list, right_list, remainder

    @staticmethod
    def get_and_add_result(page: GamePage, counter: int, weighing_list: list) -> WeightResult:
        """
        Retrieves the current weighing, adds it to the weighing list, and returns the result of the weighing.

        :param page: The GamePage instance used to interact with the game page.
        :param counter: The current count of weighing performed.
        :param weighing_list: The list of weighing collected so far.
        :return: The result of the last weighing as a WeightResult.
        """
        weighing_data = page.get_current_result(counter)
        weighing_list.append(weighing_data)
        return GameSolver.get_score(weighing_data)

    @staticmethod
    def find_the_fake(page: GamePage, digits: list, counter=0, weighing_list=None):
        """
        Recursively finds the fake bar number by performing weighing and comparing results.

        :param page: The GamePage instance used to interact with the game page.
        :param digits: The list of bar numbers to be weighed.
        :param counter: The current count of weighing performed so far.
        :param weighing_list: The list of weighing results collected so far.
        :return: A tuple containing the fake bar number, the number of weighing performed, and the list of weighing.
        """

        if weighing_list is None:
            weighing_list = []
        assert len(digits) > 0, "The list of bar numbers is empty, but it was expected to contain values."
        # base case
        if len(digits) == 1:
            return digits[0], counter, weighing_list

        left_list, right_list, remainder = GameSolver.fill_and_compare_tables(page, digits)
        counter += 1
        result = GameSolver.get_and_add_result(page, counter, weighing_list)

        if result == WeightResult.EQ:
            return remainder, counter
        elif result == WeightResult.GT:
            # recursion
            return GameSolver.find_the_fake(page, right_list, counter, weighing_list)
        else:
            return GameSolver.find_the_fake(page, left_list, counter, weighing_list)
