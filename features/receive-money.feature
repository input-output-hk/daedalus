@e2e
Feature: Receive money

  Background:
    Given I have completed the basic setup
    And I have a "Test Wallet" wallet with funds
    And I have the following wallets:
      | name          |
      | Target Wallet |

  Scenario: Hide/show used addresses
    Given I am on the "Target Wallet" wallet "receive" screen
    And I have made the following transactions:
      | source          | destination    | amount |
      | Test Wallet     | Target Wallet  | 1      |
    And I should see 1 used addresses
    When I click the ShowUsed switch
    Then I should see 19 addresses
