import pytest
from selenium import webdriver
from module.pages.balance_scale_game_pages import GamePage
from module.utils.balance_scale_game_solver import GameSolver


class TestBalanceGame:
    @pytest.fixture
    def driver(self):
        # Initialize the browser
        driver = webdriver.Chrome()
        yield driver
        # Close the browser
        driver.quit()

    def test_create_successful_balance_scale_game(self, driver):
        """
        Test automation for balance scale game to find fake gold bar.
        The test ensures that the game is played successfully, the fake gold bar is identified,
        and the required message is captured and verified.

        Steps:
        1. Initialize the game_page.
        2. Navigate to game webpage.
        3. Get all bar numbers from board.
        4. Verify that the length of bar numbers is greater than 1.
        5. Call a function that find and return the fake bar number, number of weighing, and the weighing data.
        6. Choose the fake nuber on board and get a message.
        7. Validate the message that indicates success.
        8. Print the data about the fake bar number, number of weighing, and the weighing list.

        Args:
        - driver: WebDriver instance for browser automation.
        """
        game_page = GamePage(driver)
        driver.get(game_page.url)
        digits = game_page.get_bar_numbers()
        assert len(digits) > 1, "The list of bar numbers is shorter than 2, but it was expected to contain more values."
        fake_one, number_of_weighing, weighing_list = GameSolver.find_the_fake(game_page, digits)
        message = game_page.get_message(fake_one)
        assert message == 'Yay! You find it!', f"Didn't get the right message. The actual message you got: '{message}'"
        print(
            f"The fake bar number is: {fake_one}\n Alert message: {message}\n Number of weighing: {number_of_weighing}\n List of weighing: {weighing_list} ")
