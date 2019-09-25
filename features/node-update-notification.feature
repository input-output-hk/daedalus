@e2e @watch
Feature: Node Update Notification

  Background:
    Given I have completed the basic setup

  Scenario: Application version and next update version not match
    When I set next update version to "10"
    And I set next application version to "15"
    Then I should see the node update notification overlay
    And Overlay should display "newer version" as available version and actions
  
  Scenario: Application version and next update version match
    When I set next application version to "15"
    And I set next update version to "15"
    Then I should see the node update notification overlay
    And Overlay should display "0.14.0" as available version and actions

  Scenario: User postpones a node update notification
    When I set next application version to "15"
    And I set next update version to "15"
    Then I should see the node update notification overlay
    And Overlay should display "0.14.0" as available version and actions
    When I click the postpone update button
    Then I should not see the notification component anymore

  @restartApp
  Scenario: User accepts a node update notification
    When I set next update version to "15"
    And I set next application version to "15"
    Then I should see the node update notification overlay
    And Overlay should display "0.14.0" as available version and actions
    When I click the accept update button
    Then Daedalus should quit 