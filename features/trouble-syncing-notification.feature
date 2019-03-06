Feature: Trouble Syncing Notification

  Background:
    Given I have completed the basic setup

  Scenario: The "Having trouble syncing?" notification is shown on the loading screen when block syncing stalls
    When I purposely unsync the node from the network
    Then I should see the syncing status with "Syncing blocks 50.00%"
    Then I should see the report issue notification displaying "Having trouble syncing?"