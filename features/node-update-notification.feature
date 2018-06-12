@watch
Feature: Node Update Notification

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |

  Scenario: User accepts a node update notification
    Given I am on the "Test wallet" wallet summary screen
    And when I make a node update available
    Then I should see the unexpanded node update notification
    And when I click the node update notification's toggle button
    Then I should see the expanded node update notification's message
    Then I should see the expanded node update notification's accept button
    Then I should see the expanded node update notification's postpone button
    And when I click the accept button
    Then I should not see the node update notification anymore
    And the Daedalus window should close

  Scenario: User postpones a node update notification
    Given I am on the "Test wallet" wallet summary screen
    And when I make a node update available
    Then I should see the unexpanded node update notification
    And when I click the node update notification's toggle button
    Then I should see the expanded node update notification's message
    Then I should see the expanded node update notification's accept button
    Then I should see the expanded node update notification's postpone button
    And when I click the postpone button
    Then I should not see the node update notification anymore
