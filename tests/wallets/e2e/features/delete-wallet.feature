@e2e
Feature: Delete a wallet

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test Wallet |

  Scenario: Successfully Deleting a Wallet
    Given I am on the "Test Wallet" wallet "settings" screen
    When I click on delete wallet button
    And I see delete wallet dialog
    And I click on the "Make sure you have access to backup before continuing" checkbox
    And I enter "Test Wallet" as name of the wallet to confirm
    And I submit the delete wallet dialog
    Then I should not see the delete wallet dialog anymore
