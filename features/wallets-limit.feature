@e2e
Feature: Wallet Settings

  Background:
    Given I have completed the basic setup
    Given I create wallets until I reach the maximum number permitted

  Scenario: User reaches the maximum number of wallets
    Then I should see maximum number of wallets in the wallets list
    And the buttons in the Add Wallet screen should be disabled
    And I should see a disclaimer saying I have reached the maximum number of wallets

  Scenario: User deletes one wallet and re-enable its Adding new wallets
    Given I delete the last wallet
    Then the buttons in the Add Wallet screen should be enabled
