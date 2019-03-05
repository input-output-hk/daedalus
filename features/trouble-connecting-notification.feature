Feature: Trouble Connecting Notification

  Background:
    Given I have completed the basic setup

  Scenario: Network connection lost notification is shown when node was previously connected and becomes disconnected
    When I set the node subscription status to subscribing
    Then I should see the loading screen with "Network connection lost - reconnecting"