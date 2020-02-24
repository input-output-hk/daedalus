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

  Scenario: Rewards load error handling works as expected
    Given I set rewards fetch failed
    When I click on rewards tab button
    And I am on the rewards screen
    Then I should see the following loading message:
      | message                                     |
      | staking.rewards.loadingRewardsMessage       |
    And I should not see any rewards

  Scenario: Export is working as expected
    When I click on rewards tab button
    And I am on the rewards screen
    And I click on "Export" button
    Then I should see file save dialog
