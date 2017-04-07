Feature: Select Language

  Background:
    Given I have a wallet with funds
    And the active wallet is "Personal Wallet"

  Scenario: User Selects Language
    Given I am on the language selection screen
    When I see the language selection form
    And I open language selection dropdown
    And I select Japanese language
    And I submit the language selection form
    Then I should be on the Personal Wallet summary screen
