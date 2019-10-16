@e2e @skip
# @API TODO - We don't have app update endpoints

Feature: Node Update Exit

  Background:
    Given I have completed the basic setup
    Given Daedalus is running
    And cardano-node is running

  @slow @restartApp
  Scenario: apply-update endpoint triggered, and node fails to exit, that's still handled
    When I inject fault named "FInjApplyUpdateNoExit"
    When I trigger the apply-update endpoint
    Then I should see the loading screen with "Updating Cardano node"
    And Daedalus should quit

  # TODO: clarify what should happen when cardano exits with wrong code!
  # Currently Daedalus thinks that cardano-node crashed and restarts it …
  @slow @restartApp @skip
  Scenario: apply-update endpoint triggered, and node exits with wrong exit code, that's still handled
    When I inject fault named "FInjApplyUpdateWrongExitCode"
    When I trigger the apply-update endpoint
    Then I should see the loading screen with "Updating Cardano node"
    And Daedalus should quit

  # TODO: Daedalus doesn't handle ignored api calls atm
  @slow @restartApp @skip
  Scenario: apply-update endpoint triggered, and node ignores the endpoint call, that's still handled
    Given Daedalus is running
    And cardano-node is running
    When I inject fault named "FInjIgnoreApi"
    When I trigger the apply-update endpoint
    Then cardano-node process is not running
    And Daedalus process is not running
