Feature: Quitting Daedalus

  Daedalus can be quit in multiple (and unexpected) ways and
  has to cleanup and stop cardano-node before exiting.

  @restartApp
  Scenario: Closing the main window
    Given Daedalus is running
    And cardano-node is running
    When I close the main window
    Then cardano-node process is not running
    And Daedalus process is not running

  @watch @restartApp
  Scenario: Closing the main window, while cardano ignores exit request
    Given Daedalus is running
    And cardano-node is running
    When I inject fault named "FInjIgnoreShutdown"
    When I close the main window
    Then cardano-node process is not running
    And Daedalus process is not running
