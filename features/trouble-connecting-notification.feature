Feature: Trouble Connecting Notification
  
  @slow @restartApp
  Scenario: Trouble connecting notification is shown when node is not connected
    Given Daedalus is running
    When I refresh the main window
    Then I should see the loading screen with "Connecting to network"
    When I set the node to disconnected
    Then I should see the loading screen with report connecting issue text "Having trouble connecting to network?"
  