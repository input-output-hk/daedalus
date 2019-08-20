@e2e
Feature: Receive money

  Background:
    Given I have completed the basic setup
    And I have a "Imported Wallet" with funds
    And I have the following wallets:
      | name         |
      | TargetWallet |

  Scenario: Hide/show used addresses
    Given I am on the "TargetWallet" wallet "receive" screen
    And I generate 1 addresses
    And I have made the following transactions:
      | source          | destination   | amount |
      | Imported Wallet | TargetWallet  | 1      |
    Then I should see 2 addresses
    And I should see 1 used addresses
    When I click the ShowUsed switch
    Then I should see 1 addresses

  Scenario: Addresses ordering
    Given I am on the "TargetWallet" wallet "receive" screen
    And I generate 2 addresses
    Then I should see the following addresses:
      | ClassName          |
      | generatedAddress-1 |
      | generatedAddress-2 |
      | generatedAddress-3 |
    And The active address should be the newest one
