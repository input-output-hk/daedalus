@e2e
Feature: Node Update Notification

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |

  Scenario: Application version and next update version not match
    When I set next application update version to "10"
    Then I should see the node update notification overlay
    And Overlay should display "newer version" as available version and actions
    And I should see the accept update button
    And I should see the postpone update button
  
  Scenario: Application version and next update version match
    When I set next application update version to "15"
    Then I should see the node update notification overlay
    And Overlay should display "0.14.0" as available version and actions
    And I should see the accept update button
    And I should see the postpone update button

  Scenario: User postpones a node update notification
    Then I should see the node update notification overlay
    When I click the postpone update button
    Then I should not see the notification component anymore

  @restartApp
  Scenario: User accepts a node update notification
    Then I should see the node update notification overlay
    When I click the accept update button
    Then Daedalus should quit 