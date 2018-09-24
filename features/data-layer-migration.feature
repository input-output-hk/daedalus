Feature: Data Layer Migration

  Background:
    Given I haven't clicked the migration button
    And I have completed the basic setup

  Scenario: I don't have any wallets
    Then I should not see the Data Layer Migration screen

  Scenario: I do have wallets
    Given I have the following wallets, skiping the data layer migration:
    | name   | password  |
    | Wallet |           |
    Then I should see the Data Layer Migration screen
    When I click the migration button
    Then I should see the initial screen
    When I refresh the application
    Then I should not see the Data Layer Migration screen
