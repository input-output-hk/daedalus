Feature: Update Node

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |

  Scenario: User accepts a node update notification
    Given I am on the wallet summary screen
    And I see the NodeUpdateNotification unexpanded
    And I click on the downward arrow expand button
    Then I should see the NodeUpdateNotification's message + update/postpone buttons
    And when I click the NodeUpdateNotification's update button
    Then I should not see the NodeUpdateNotification anymore
    And the Daedalus window should close

  Scenario: User denies a node update notification
    Given I am on the wallet summary screen
    And I see the NodeUpdateNotification unexpanded
    And I click on the downward arrow expand button
    Then I should see the NodeUpdateNotification's message + update/postpone buttons
    And when I click the NodeUpdateNotification's postpone button
    Then I should not see the NodeUpdateNotification anymore
