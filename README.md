## Overview
This project is designed for testing the Balance Scale game using Selenium WebDriver. 
It includes helper methods for interacting with the game page and determining the results of weighing. 
The automation is designed to be adaptable, allowing changes to the bar numbers and their length. 
The algorithm runs recursively to find the fake gold bar with minimal weighing. This approach ensures efficiency and 
accuracy in identifying the fake bar, making the automation robust and flexible to adjustments in the game settings.

## Installation
### Prerequisites
- Ensure that the WebDriver (e.g., ChromeDriver) is installed and its path is properly configured.
- Modify the test data or configuration files as needed for your specific environment.

### Usage
1. Clone the repository.
2. Install the required packages:
    ```sh
    pip install -r requirements.txt
    ```
3. Ensure that ChromeDriver is in your PATH or specify the path in your test setup.
4. Run the test using pytest:
    ```sh
    pytest test_scenarios/balance_scale_game_test.py -s 
    ```
5. View test results.

## Project Structure
### modules: 
  - `locators`: Contains locator classes for the game page elements.
  - `pages`: Contains page object classes with methods to interact with the game page.
  - `utils`: Contains utility classes and methods for solving the game.
### Test Scenarios: 
  - `test_scenarios`: Contains test cases for the Balance Scale game.

## Dependencies
- Python 3.x
- Selenium WebDriver
- pytest
