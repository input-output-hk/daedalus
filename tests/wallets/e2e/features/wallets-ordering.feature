@e2e
Feature: Wallets ordering

  Background:
    Given I have completed the basic setup
    Given I have a "Byron Wallet" byron wallet

  Scenario: "Daedalus Byron" wallet is shown on the bottom of the list below "Daedalus Shelley" wallet in order of creation
    And I have created the following wallets:
      | name     |
      | Wallet 1 |
      | Wallet 2 |
      | Wallet 3 |
      | Wallet 4 |
      | Wallet 5 |
    Then I should see the wallets in the following order:
      | name           |
      | Wallet 1       |
      | Wallet 2       |
      | Wallet 3       |
      | Wallet 4       |
      | Wallet 5       |
      | Byron Wallet |

  Scenario: Wallets are shown in order of creation
    And I have created the following wallets:
      | name     |
      | Wallet 1 |
      | Wallet 2 |
      | Wallet 3 |
      | Wallet 4 |
      | Wallet 5 |
    Then I should see the wallets in the following order:
      | name           |
      | Byron Wallet |
      | Wallet 1       |
      | Wallet 2       |
      | Wallet 3       |
      | Wallet 4       |
      | Wallet 5       |
