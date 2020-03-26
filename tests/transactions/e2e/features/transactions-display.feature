@e2e
Feature: Display wallet transactions

  The wallet summary screen displays the five most recent transactions,
  sorted by creation date (newest first). If there are more than five
  transactions it shows a "show more transactions" button to redirect
  to the wallet transactions screen where all are listed.

  Background:
    Given I have completed the basic setup
    And I have a "Test Wallet" wallet with funds
    And I have the following "Rewards" wallets:
      | name          |
      | Target Wallet |

  Scenario: No recent transactions
    When I am on the "Target Wallet" wallet "summary" screen
    Then I should not see any transactions
    And I should see the no recent transactions message
    When I am on the "Target Wallet" wallet "transactions" screen
    Then I should not see any transactions
    And I should see the no recent transactions message

  @skip
  # @API TODO - all transactions get included in the same block and have the same timestamp
  Scenario: More than five transactions
    Given I have made the following transactions:
      | source      | destination    | amount |
      | Test Wallet | Target Wallet  | 1      |
      | Test Wallet | Target Wallet  | 2      |
      | Test Wallet | Target Wallet  | 3      |
      | Test Wallet | Target Wallet  | 4      |
      | Test Wallet | Target Wallet  | 5      |
      | Test Wallet | Target Wallet  | 6      |
    When I am on the "Target Wallet" wallet "summary" screen
    Then I should see the following transactions:
      | type   | amount |
      | income | 6      |
      | income | 5      |
      | income | 4      |
      | income | 3      |
      | income | 2      |
    When I click on the show more transactions button
    Then I should be on the "Target Wallet" wallet "transactions" screen
    Then I should see the following transactions:
      | type   | amount |
      | income | 6      |
      | income | 5      |
      | income | 4      |
      | income | 3      |
      | income | 2      |
      | income | 1      |
