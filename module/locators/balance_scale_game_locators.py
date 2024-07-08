from selenium.webdriver.common.by import By


class GameLocators:
    """
     A collection of locators for elements on the game page
    """

    RESET_BTN = (By.XPATH, "/html/body/div/div/div[1]/div[4]/button[1]")
    WEIGHT_BTN = (By.ID, "weigh")
    COINS = (By.CLASS_NAME, "coins")
    COINS_BTN = (By.CSS_SELECTOR, ".coins button")
    WEIGHING_DATA = "//*[@id='root']/div/div[1]/div[5]/ol/li[{}]"
    COIN_ID = "coin_{}"
    CELL_ID = "{}_{}"
