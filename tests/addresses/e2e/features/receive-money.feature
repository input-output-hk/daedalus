@e2e
Feature: Receive money

  Background:
    Given I have completed the basic setup
    And I have a "Test Wallet" wallet with funds
    And I have the following "Rewards" wallets:
      | name          |
      | Target Wallet |

  @shelley
  Scenario: Hide/show used addresses
    Given I am on the "Target Wallet" wallet "receive" screen
    And I have made the following transactions:
      | source          | destination    | amount |
      | Test Wallet     | Target Wallet  | 1      |
    And I should see 1 used addresses
    When I click the ShowUsed switch
    Then I should not see any used addresses

  @api-wip
  Scenario: Hide/show used addresses
    Given I am on the "Target Wallet" wallet "receive" screen
    And I enter wallet password in generate address input field "Secret1234"
    And I generate 1 addresses
    And I have made the following transactions:
      | source          | destination   | amount |
      | Test Wallet     | Target Wallet  | 1     |
    Then I should see 2 addresses
    And I should see 1 used addresses
    When I click the ShowUsed switch
    Then I should see 1 addresses

  @api-wip
  Scenario: Addresses ordering
    Given I am on the "Test Wallet" wallet "receive" screen
    And I enter wallet password in generate address input field "Secret1234"
    And I generate 2 addresses
    Then I should see the following addresses:
      | ClassName          |
      | generatedAddress-1 |
      | generatedAddress-2 |
      | generatedAddress-3 |
    And The active address should be the newest one