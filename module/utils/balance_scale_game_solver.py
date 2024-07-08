from module.pages.balance_scale_game_pages import GamePage
from module.utils.balance_scale_weigh_result import WeightResult


class GameSolver:

    @staticmethod
    def get_score(weighing) -> WeightResult:
        if WeightResult.EQ.value in weighing:
            return WeightResult.EQ
        elif WeightResult.GT.value in weighing:
            return WeightResult.GT
        else:
            return WeightResult.LT

    @staticmethod
    def find_the_fake(page: GamePage, digits: list, counter=0, weighing_list=None):
        if weighing_list is None:
            weighing_list = []

        assert len(digits) > 0, "The list of bar numbers is empty, but it was expected to contain values."

        page.click_reset_button()

        # base case
        if len(digits) == 1:
            return digits[0], counter, weighing_list

        # extract one number
        remainder = None if len(digits) % 2 == 0 else digits.pop()

        left_set = digits[:len(digits) // 2]
        right_set = digits[len(digits) // 2:]

        page.enter_digit_to_table("left", left_set)
        page.enter_digit_to_table("right", right_set)

        page.click_weight_button()
        counter += 1
        weighing_data = page.get_current_result(counter)
        weighing_list.append(weighing_data)
        result = GameSolver.get_score(weighing_data)

        if result == WeightResult.EQ:
            return remainder, counter
        elif result == WeightResult.GT:
            # recursion
            return GameSolver.find_the_fake(page, right_set, counter, weighing_list)
        else:
            return GameSolver.find_the_fake(page, left_set, counter, weighing_list)
