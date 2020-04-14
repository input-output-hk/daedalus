@e2e @skip
Feature: Data Layer Migration

  Background:
    Given I haven't accepted the data layer migration

  Scenario: I don't have any wallets
    Given I have selected English language
    And I have accepted "Terms of use"
    Then I should not see the Data Layer Migration screen

  Scenario: I do have wallets
    Given I have the following wallets:
    | name   |
    | Wallet |
    And I have selected English language
    And I have accepted "Terms of use"
    Then I should see the Data Layer Migration screen
    When I click the migration button
    Then I should see the main ui
    When I refresh the main window
    Then I should not see the Data Layer Migration screen
