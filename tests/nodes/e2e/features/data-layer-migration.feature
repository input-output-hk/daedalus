@e2e
Feature: Data Layer Migration

  Background:
    Given I haven't accepted the data layer migration
    And I have selected English language
    And I have accepted "Terms of use"

  Scenario: I don't have any wallets
    Then I should not see the Data Layer Migration screen

  Scenario: I do have wallets
    Given I have the following wallets:
    | name   |
    | Wallet |
    Then I should see the Data Layer Migration screen
    When I click the migration button
    Then I should see the main ui
    When I refresh the main window
    Then I should not see the Data Layer Migration screen
