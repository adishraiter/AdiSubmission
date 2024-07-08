from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from module.locators.balance_scale_game_locators import GameLocators
from selenium.webdriver.common.by import By


class GamePage:
    """
    Helper methods that act and interact with the game page.
    """

    _GAME_URL = "http://sdetchallenge.fetch.com/"

    def __init__(self, driver, delay=3, url=_GAME_URL):
        """
        driver: WebDriver instance for browser automation.
        delay: used for explicit waits.
        url:The URL of the game page.
        """

        self.driver = driver
        self.wait = WebDriverWait(self.driver, delay)
        self.url = url

    def click_reset_button(self):
        """
        Clicks the "Reset" button on the game page.
        """

        self.wait.until(EC.element_to_be_clickable(GameLocators.RESET_BTN)).click()

    def click_weight_button(self):
        """
        Clicks the "Weigh" button on the game page.
        """

        self.wait.until(EC.element_to_be_clickable(GameLocators.WEIGHT_BTN)).click()

    def get_current_result(self, counter: int) -> str:
        """
         Retrieves the last result data.
        :param counter: number of weighing.
        :return: the tables data and the result of the last weighing.
        """

        weighting_data = GameLocators.WEIGHING_DATA.format(counter)
        return self.wait.until(EC.presence_of_element_located((By.XPATH, weighting_data))).text

    def get_bar_numbers(self) -> list:
        """
        Retrieves all possible bar numbers from the game board and extracts them into a list.
        :return: A list of all the bar numbers.
        """

        self.wait.until(EC.presence_of_element_located(GameLocators.COINS))
        buttons = self.driver.find_elements(*GameLocators.COINS_BTN)
        button_texts = [button.text for button in buttons]
        return button_texts

    def enter_digit_to_table(self, side: str, digits: list):
        """
        Enters a list of digits into the specified side of the table on the game page.
        :param side: The side of the table to insert the numbers: 'left' or 'right'.
        :param digits: The list of digits to insert into the table.
        """

        for i, digit in enumerate(digits):
            cell_id = GameLocators.CELL_ID.format(side, i)
            cell_element = self.driver.find_element(By.ID, cell_id)
            cell_element.send_keys(digit)

    def get_message(self, fake_number: str) -> str:
        """
        Click on the number button and retrieve the pop-up message text.
        :param fake_number: The fake bar number.
        :return: The message displayed at the end of the game.
        """
        coin_locator = GameLocators.COIN_ID.format(fake_number)
        self.driver.find_element(By.ID, coin_locator).click()
        alert_text = self.wait.until(EC.alert_is_present()).text
        return alert_text
