Feature: Generate Wallet Address

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"

  Scenario: Generating wallet address
    Given I have the following wallets:
    | name   |
    | first  |
    And I am on the "first" wallet "receive" screen
    And I have one wallet address
    And I click on the "Generate new address" button
    Then I should see two wallet addresses

  Scenario: Generating wallet address for a wallet with spending password
    Given I have the following wallets:
    | name   | password |
    | first  | secret   |
    And I am on the "first" wallet "receive" screen
    And I have one wallet address
    And I enter spending password "secret"
    And I click on the "Generate new address" button
    Then I should see two wallet addresses
