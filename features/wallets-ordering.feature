Feature: Wallet Odering

  Background:
    Given I have completed the basic setup

  Scenario: Wallets ordering
    Given I have created the following wallets:
      | name     |
      | Wallet 1 |
      | Wallet 2 |
      | Wallet 3 |
    Then I should see the wallets in the following order:
      | name     |
      | Wallet 1 |
      | Wallet 2 |
      | Wallet 3 |
