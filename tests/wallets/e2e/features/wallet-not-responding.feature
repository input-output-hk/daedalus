@e2e
Feature: Wallet is not responding

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name     |
      | Wallet 1 |
      | Wallet 2 |

  Scenario: Successfully displaying the Not Responding Overlay
    Given I am on the "Wallet 1" wallet "transactions" screen
    When the "Wallet 1" wallet is not responding
    Then the "Not Responding" Overlay should be visible
    And the wallet navigation should switch to the "summary" tab

  Scenario: Hide the overlay on wallets with no error
    Given I am on the "Wallet 1" wallet "summary" screen
    When the "Wallet 1" wallet is not responding
    Then the "Not Responding" Overlay should be visible
    When I am on the "Wallet 2" wallet "summary" screen
    Then the "Not Responding" Overlay should be hidden
    When I am on the "Wallet 1" wallet "summary" screen
    Then the "Not Responding" Overlay should be visible
