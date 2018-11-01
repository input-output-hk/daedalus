@watch
Feature: Node Update Notification

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |
    When I am on the "Test wallet" wallet "summary" screen
    When I make a node update available
    Then I should see the node update notification component
    Then I should see the notification's title bar
    Then I should see the expected update version in the notification's title bar
    Then I should see the notification's toggle button
    Then I should see the notification's update message
    Then I should see the notification's accept button
    Then I should see the notification's postpone button

  Scenario: User postpones a node update notification
    When I click the notification's postpone button
    Then I should not see the notification component anymore

  @restartApp
  Scenario: User accepts a node update notification
    When I click the notification's accept button
    Then I should see the Daedalus window close
    And I should see Daedalus process not running
