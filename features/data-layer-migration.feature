Feature: Data Layer Migration

  Background:
    Given I haven't clicked the migration button
    And I have selected English language
    And I have accepted "Terms of use"

  # Scenario: I don't have any wallets
  #   Given I delete all wallets
  #   When I refresh the application
  #   Then I should not see the Data Layer Migration screen

  Scenario: I do have wallets
    Given I have created the following wallets:
    | name   | password  |
    | Wallet |           |
    Then I should see the Data Layer Migration screen
    When I click the migration button
    Then I should go to the initial screen
