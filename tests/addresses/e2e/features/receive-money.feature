@e2e
Feature: Receive money

  Background:
    Given I have completed the basic setup
    And I have a "Test Wallet" wallet with funds
    And I have the following wallets:
      | name          |
      | Target Wallet |

  @shelley
  Scenario: Hide/show "Shelley" wallet used addresses
    Given I am on the "Target Wallet" wallet "receive" screen
    And I have made the following transactions:
      | source          | destination    | amount |
      | Test Wallet     | Target Wallet  | 1      |
    And I should see 1 used addresses
    When I click the ShowUsed switch
    Then I should not see any used addresses

  @byron
  Scenario: Hide/show "Byron" wallet used addresses
    Given I am on the "Target Wallet" wallet "receive" screen
    And I enter wallet password in generate address input field "Secret1234"
    And I have 1 generated wallet addresses
    And I have made the following transactions:
      | source          | destination   | amount |
      | Test Wallet     | Target Wallet  | 1     |
    Then I should see 2 addresses
    And I should see 1 used addresses
    When I click the ShowUsed switch
    Then I should see 1 addresses

  @byron
  Scenario: Byron Wallet addresses ordering
    Given I am on the "Target Wallet" wallet "receive" screen
    And I enter wallet password in generate address input field "Secret1234"
    And I create 2 addresses
    Then I should see the following addresses:
      | ClassName          |
      | generatedAddress-1 |
      | generatedAddress-2 |
      | generatedAddress-3 |
    And The active address should be the newest one

  @byron
  Scenario: Generate "Byron" wallet address with wrong passphrase
    Given I am on the "Target Wallet" wallet "receive" screen
    And I enter wallet password in generate address input field "wrong1234"
    And I click "Generate a new address" button
    Then I should see the following error messages on the wallet receive screen:
      | message                           |
      | api.errors.IncorrectPasswordError |

  @byron
  Scenario: Generate "Byron" wallet address with too short passphrase
    Given I am on the "Target Wallet" wallet "receive" screen
    And I enter wallet password in generate address input field "wrong"
    And I click "Generate a new address" button
    Then I should see the following error messages on the wallet receive screen:
      | message                           |
      | api.errors.IncorrectPasswordError |
