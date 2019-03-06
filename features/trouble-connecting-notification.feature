Feature: Trouble Connecting Notification

  Background:
    Given I have completed the basic setup

  Scenario: The "Having trouble connecting to network?" notification is shown on loading screen when previously connected node becomes disconnected
    When I set the node subscription status to subscribing
    Then I should see the loading screen with "Network connection lost - reconnecting"
    Then I should see the report issue notification displaying "Having trouble connecting to network?"