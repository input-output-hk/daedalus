Feature: Import Wallet via Sidebar

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |

  Scenario: Successfully Importing a Wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallet dialog
    And I select a valid wallet import key file
    And I click on the import wallet button in import wallet dialog
    Then I should not see the import wallet dialog anymore
    And I should have newly created "Imported Wallet" wallet loaded
    And I should be on the "Imported Wallet" wallet "summary" screen
    And I should see the restore status notification while import is running
    And I should not see the restore status notification once import is finished

  Scenario: Wallet Already Imported Error
    Given I have a "Imported Wallet" with funds
    When I try to import the wallet with funds again
    Then I see the import wallet dialog with an error that the wallet already exists

  @skip
  Scenario: Successfully Importing a Wallet with spending password
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallet dialog
    And I select a valid wallet import key file
    And I toggle "Activate to create password" switch on the import wallet key dialog
    And I should see wallet spending password inputs
    And I enter wallet spending password:
      | password  | repeatedPassword |
      | Secret123 | Secret123        |
    And I click on the import wallet button in import wallet dialog
    Then I should not see the import wallet dialog anymore
    And I should have newly created "Imported Wallet" wallet loaded
    And I should be on the "Imported Wallet" wallet "summary" screen
    And I should see the restore status notification while import is running
    And I should not see the restore status notification once import is finished
