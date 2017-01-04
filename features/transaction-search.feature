Feature: Transaction Search
  In order to find transaction with specific words in the title
  As a User
  I want to be able to enter my search terms and have the transactions list filtered

  Background:
    Given I have an account
    And I have a wallet
    And I made the following transactions with my wallet:
      | title  |
      | First  |
      | Second |
      | Third  |
    And I am logged in
    And I am on the wallet home screen

  Scenario: Filtering Down to a Single Transaction
    Given I see all expected transactions on screen
    When I enter "First" into the transaction search
    Then I should only see the following transactions:
    | title |
    | First |
