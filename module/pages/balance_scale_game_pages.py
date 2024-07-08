from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from module.locators.balance_scale_game_locators import GameLocators
from selenium.webdriver.common.by import By


class GamePage:

    def __init__(self, driver, delay=3):
        self.driver = driver
        self.wait = WebDriverWait(self.driver, delay)
        self.url = "http://sdetchallenge.fetch.com/"

    def click_reset_button(self):
        self.wait.until(EC.element_to_be_clickable(GameLocators.RESET_BTN)).click()

    def click_weight_button(self):
        self.wait.until(EC.element_to_be_clickable(GameLocators.WEIGHT_BTN)).click()

    def get_current_result(self, counter):
        weighting_data = f"//*[@id='root']/div/div[1]/div[5]/ol/li[{counter}]"
        return self.wait.until(EC.presence_of_element_located((By.XPATH, weighting_data))).text

    def enter_digit_to_table(self, side, digits):
        for i, digit in enumerate(digits):
            cell_id = f"{side}_{i}"
            cell_element = self.driver.find_element(By.ID, cell_id)
            cell_element.send_keys(str(digit))

    def get_message(self, fake_number):
        coin_locator = f"coin_{fake_number}"
        self.driver.find_element(By.ID, coin_locator).click()
        alert_text = self.wait.until(EC.alert_is_present()).text
        return alert_text

    def get_bar_numbers(self):
        self.wait.until(EC.presence_of_element_located(GameLocators.COINS))
        # Find all button elements within the container
        buttons = self.driver.find_elements(*GameLocators.COINS_BTN)

        # Extract and return the text of each button
        button_texts = [int(button.text) for button in buttons]
        return button_texts
