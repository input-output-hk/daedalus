@e2e @skip
# @API TODO - We don't have api endpoint for fetching node info

Feature: Trouble Syncing Notification

  Background:
    Given I have completed the basic setup

  Scenario: The "Having trouble syncing?" notification is shown on the loading screen when block syncing stalls
    When I arbitrarily set the local block height to half the network block height
    Then I should see the syncing status with "Syncing blocks 50.00%"
    Then I should see the report issue notification displaying "Having trouble syncing?"
    And The report issue button should be visible
    When I reconnect local and network block heights to the node
    Then I should not see the report issue notification
    And The report issue button should be hidden
