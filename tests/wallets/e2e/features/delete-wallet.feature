@e2e
Feature: Delete a wallet

  Background:
    Given I have completed the basic setup
    And I have the following "Rewards" wallets:
      | name           |
      | Rewards Wallet |
    And I have a "Balance Wallet" balance wallet

  @byron
  Scenario: Successfully deleting "Daedalus Balance" wallet from settings screen
    And I am on the "Balance Wallet" wallet "settings" screen
    When I click on delete wallet button
    And I see delete wallet dialog
    And I click on the "Make sure you have access to backup before continuing" checkbox
    And I enter "Balance Wallet" as name of the wallet to confirm
    And I submit the delete wallet dialog
    Then I should not see the delete wallet dialog anymore

  Scenario: Successfully deleting "Daedalus Rewards" wallet from settings screen
    Given I am on the "Rewards Wallet" wallet "settings" screen
    When I click on delete wallet button
    And I see delete wallet dialog
    And I click on the "Make sure you have access to backup before continuing" checkbox
    And I enter "Rewards Wallet" as name of the wallet to confirm
    And I submit the delete wallet dialog
    Then I should not see the delete wallet dialog anymore
