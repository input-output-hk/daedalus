@e2e
Feature: Node Update Notification

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |
    When I am on the "Test wallet" wallet "summary" screen
    When I make newer application version available
    When I make a node update available

  Scenario: Notification shows up when update available
    Then I should see the node update notification overlay
    And Overlay should display the versions info
    And I should see the accept update button
    And I should see the postpone update button

  Scenario: User postpones a node update notification
    Then I should see the node update notification overlay
    When I click the postpone update button
    Then I should not see the notification component anymore

  Scenario: User close node update notification
    Then I should see the node update notification overlay
    When I click the close notification button
    Then I should not see the notification component anymore

  @restartApp
  Scenario: User accepts a node update notification
    Then I should see the node update notification overlay
    When I click the accept update button
    Then Daedalus should quit