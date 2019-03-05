Feature: Trouble Connecting Notification

  Background:
    Given I have completed the basic setup

  Scenario: The "Having trouble syncing?" notification is shown on the loading screen when block syncing stalls
    When I purposely unsync the node from the network
    Then I should see the loading screen with "Syncing blocks"
    Then I should wait 5 seconds and see the trouble syncing notification displaying "Having trouble syncing?"