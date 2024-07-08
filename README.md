## Overview
This project is designed for testing the Balance Scale game using Selenium WebDriver. 
It includes helper methods for interacting with the game page and determining the results of weighings. 
The automation is designed to be adaptable, allowing changes to the bar numbers and their length. 
The algorithm runs recursively to find the fake gold bar with minimal weighings. This approach includes:
- Getting all the bar numbers from the game board and saving them in a list.
- Checking if the length is an odd number, if it is, taking out the last digit and saving it.
- Dividing the remaining list into two equal lists and filling each one into a table.
- Performing a weighing and checking the result:
- If the result is "=", the saved number is identified as the fake one.
- If not, the algorithm retrieves the result and takes the list with the lower weight for further processing.
- Recursively continuing the process until the fake number is found.

This approach ensures efficiency and accuracy in identifying the fake bar, 
making the automation robust and flexible to adjustments in the game settings.

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
