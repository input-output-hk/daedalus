@e2e
Feature: Trouble Connecting Notification

  Background:
    Given I have completed the basic setup

  Scenario: The "Having trouble connecting to network?" notification is shown on loading screen when the node remains disconnected longer than the maximum time limit
    When I set the node subscription status to subscribing
    Then I should see the loading screen with "Network connection lost - reconnecting"
    Then I should see the report issue notification displaying "Having trouble connecting to network?"
    And The report issue button should be visible
    When I set the node subscription status to subscribed
    Then I should not see the loading screen
    And The report issue button should be hidden

  Scenario: The "Having trouble connecting to network?" notification is shown on loading screen when both local and network block heights remain equal past the maximum time limit
    When I set both local and network block heights to a static, equal number
    Then I should see the loading screen with "Network connection lost - reconnecting"
    Then I should see the report issue notification displaying "Having trouble connecting to network?"
    And The report issue button should be visible
    When I reconnect local and network block heights to the node
    Then I should not see the report issue notification
    And The report issue button should be hidden
