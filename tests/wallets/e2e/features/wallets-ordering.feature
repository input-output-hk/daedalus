@e2e @watch
Feature: Wallets ordering

  Background:
    Given I have completed the basic setup
    Given I have a "Balance Wallet" balance wallet

  Scenario: "Daedalus Balance" wallet is shown on the bottom of the list below "Daedalus Rewards" wallet in order of creation
    And I have created the following "Rewards" wallets:
      | name     |
      | Wallet 1 |
      | Wallet 2 |
      | Wallet 3 |
      | Wallet 4 |
      | Wallet 5 |
    Then I should see the wallets in the following order:
      | name     |
      | Wallet 1 |
      | Wallet 2 |
      | Wallet 3 |
      | Wallet 4 |
      | Wallet 5 |
      | Balance Wallet |
