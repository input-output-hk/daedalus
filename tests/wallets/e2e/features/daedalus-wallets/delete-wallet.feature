@e2e
Feature: Delete Daedalus wallet

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test Wallet |

  Scenario: Successfully Deleting "Daedalus Balance" wallet from settings screen
  	When I restore "Daedalus Balance Wallet" balance wallet with funds
    And I am on the "Daedalus Balance Wallet" wallet "settings" screen
    When I click on delete wallet button
    And I see delete wallet dialog
    And I click on the "Make sure you have access to backup before continuing" checkbox
    And I enter "Daedalus Balance Wallet" as name of the wallet to confirm
    And I submit the delete wallet dialog
    Then I should not see the delete wallet dialog anymore

  Scenario: Delete "Daedalus Rewards" wallet from settings screen
  	When I restore "Rewards Wallet" wallet with funds
    And I am on the "Rewards Wallet" wallet "settings" screen
    When I click on delete wallet button
    And I see delete wallet dialog
    And I click on the "Make sure you have access to backup before continuing" checkbox
    And I enter "Rewards Wallet" as name of the wallet to confirm
    And I submit the delete wallet dialog
    Then I should not see the delete wallet dialog anymore
    

 