@e2e
Feature: Quitting Daedalus

  Daedalus can be quit in multiple (and unexpected) ways and
  has to cleanup and stop cardano-node before exiting.

  @slow @restartApp
  Scenario: Closing the main window
    Given Daedalus is running
    And cardano-node is running
    When I close the main window
    Then cardano-node process is not running
    And Daedalus process is not running

  @slow @restartApp
  Scenario: Closing the main window, while cardano ignores exit request
    Given Daedalus is running
    And cardano-node is running
    When I inject fault named "FInjIgnoreShutdown"
    And I close the main window
    Then I should see the loading screen with "Stopping Cardano node"
    And cardano-node process is not running
    And Daedalus process is not running
