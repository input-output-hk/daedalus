@skip
Feature: Select Language

  Background:
    Given I have a wallet with funds
    And the active wallet is "Personal Wallet"

  Scenario: User Selects Language
    Given I am on the language selection screen
    When I submit the language selection form
    Then I should be on the "Personal Wallet" wallet "summary" screen
