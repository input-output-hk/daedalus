@e2e
Feature: Wallet is not responding

  Background:
    Given I have completed the basic setup
    And I have the following "Rewards" wallets:
      | name        |
      | Test Wallet |

  Scenario: Successfully displaying the Not Responding Overlay
    Given I am on the "Test Wallet" wallet "transactions" screen
    When the "Test Wallet" wallet is not responding
    Then I should see the "Not Responding" Overlay
    And the wallet navigation should switch to the "summary" tab
