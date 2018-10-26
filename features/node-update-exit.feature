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

  @restartApp @skip
  Scenario: User accepts a node update notification
    When I click the notification's accept button
    Then I should see the Daedalus window close

  @restartApp
  Scenario: apply-update endpoint triggered, and node fails to exit, that's still handled
    Given Daedalus is running
    And cardano-node is running
    When I inject fault named "FInjApplyUpdateNoExit"
    When I trigger the apply-update endpoint
    Then cardano-node process is not running
    And Daedalus process is not running

  @restartApp
  Scenario: apply-update endpoint triggered, and node exits with wrong exit code, that's still handled
    Given Daedalus is running
    And cardano-node is running
    When I inject fault named "FInjApplyUpdateWrongExitCode"
    When I trigger the apply-update endpoint
    Then cardano-node process is not running
    And Daedalus process is not running

  @restartApp
  Scenario: apply-update endpoint triggered, and node ignores the endpoint call, that's still handled
    Given Daedalus is running
    And cardano-node is running
    When I inject fault named "FInjIgnoreApi"
    When I trigger the apply-update endpoint
    Then cardano-node process is not running
    And Daedalus process is not running
