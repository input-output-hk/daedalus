@e2e @skip
Feature: Trouble Connecting Notification

  Background:
    Given I have completed the basic setup

  Scenario: The "Having trouble connecting to network?" notification is shown on loading screen when the syncing percentage remains unchanged longer than the maximum time limit
    Given I set the syncing progress to 0 percent
    Then I should see the loading screen with "Network connection lost - reconnecting"
    Then I should see the report issue notification displaying "Having trouble connecting to network?"
    And The report issue button should be visible
    When I reset the syncing progress
    Then I should not see the loading screen
    And The report issue button should be hidden
