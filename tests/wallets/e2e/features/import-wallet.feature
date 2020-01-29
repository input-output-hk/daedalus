@e2e @skip
# @API TODO - We don't have API endpoint for import wallet from key
Feature: Import a wallet

  Background:
    Given I have completed the basic setup
    And I have the following "Rewards" wallets:
      | name        |
      | Test Wallet |

  Scenario: Successfully Importing a Wallet with spending password
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallet dialog
    And I select a valid wallet import key file
    And I should see wallet spending password inputs
    And I enter wallet spending password:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click on the import wallet button in import wallet dialog
    Then I should not see the import wallet dialog anymore
    And I should have newly created "Test Wallet" wallet loaded
    And I should be on the "Test Wallet" wallet "summary" screen
    And I should see the restore status notification while import is running
    And I should not see the restore status notification once import is finished

  Scenario: Wallet Already Imported Error
    Given I have a "Test Wallet" rewards wallet with funds
    When I try to import the wallet with funds again
    Then I see the import wallet dialog with an error that the wallet already exists