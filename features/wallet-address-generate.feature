@e2e
Feature: Generate Wallet Address

  Background:
    Given I have completed the basic setup

  Scenario: Generating wallet address
    Given I have the following wallets:
      | name   |
      | first  |
    And I am on the "first" wallet "receive" screen
    And I have one wallet address
    And I click on the "Generate new address" button
    Then I should see newly generated address as active address on the wallet receive screen

  Scenario: Generating wallet address for a wallet with spending password
    Given I have the following wallets:
      | name   | password  |
      | first  | Secret123 |
    And I am on the "first" wallet "receive" screen
    And I have one wallet address
    And I enter spending password "Secret123"
    And I click on the "Generate new address" button
    Then I should see newly generated address as active address on the wallet receive screen
