@e2e
Feature: Rewards

  Background:
    Given I have completed the basic setup
    And I have a "Test Wallet" rewards wallet with funds
    And I am on the Delegation Center screen
    And I mark experimental feature as read

  Scenario: Rewards are correctly loaded
    When I click on rewards tab button
    And I am on the rewards screen
    Then I should see rewards listed

  Scenario: "No rewards" are correctly displayed with appropriate label
    Given I set rewards as an empty result
    When I click on rewards tab button
    And I am on the rewards screen
    Then I should see no rewards label
