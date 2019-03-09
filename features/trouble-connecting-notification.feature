Feature: Trouble Connecting Notification

  Background:
    Given I have completed the basic setup

  Scenario: The "Having trouble connecting to network?" notification is shown on loading screen when previously connected node becomes disconnected and remains so longer than the maximum time limit
    When I set the node subscription status to subscribing
    Then I should see the loading screen with "Network connection lost - reconnecting"
    Then I should see the report issue notification displaying "Having trouble connecting to network?"
    When I set the node subscription status to subscribed
    Then I should not see loading screen anymore

  Scenario: The "Having trouble connecting to network?" notification is shown on loading screen when both network and node's block heights remain the same longer than the maximum time limit
    When I purposely set the network and local block heights to a static and equal number
    Then I should see the loading screen with "Network connection lost - reconnecting"