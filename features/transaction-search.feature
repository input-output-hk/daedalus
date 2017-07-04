@skip
Feature: Transaction Search
  In order to find transaction with specific words in the title
  As a User
  I want to be able to enter my search terms and have the transactions list filtered

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"
    And I agree to send logs to remote server
    And I have a wallet
    And I made the following transactions with my wallet:
      | title  |
      | First  |
      | Second |
      | Third  |
    And I am on the wallet summary screen

  Scenario: Filtering Down to a Single Transaction
    Given I see all expected transactions on screen
    When I enter "First" into the transaction search
    Then I should only see the following transactions:
    | title |
    | First |
