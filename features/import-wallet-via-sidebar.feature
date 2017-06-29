Feature: Import Wallet via Sidebar

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"
    And I have the following wallets:
    | name  |
    | first |

  Scenario: Successfully Importing a Wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet dialog
    And I click on the import wallet button in add wallet dialog
    And I see the import wallet dialog
    And I select a valid wallet import key file
    And I click on the import wallet button in import wallet dialog
    Then I should not see the import wallet dialog anymore
    And I should have newly created "Genesis wallet" wallet loaded
    And I should be on the "Genesis wallet" wallet "summary" screen

  Scenario: Successfully Importing a Wallet with spending password
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet dialog
    And I click on the import wallet button in add wallet dialog
    And I see the import wallet dialog
    And I select a valid wallet import key file
    And I toggle "Activate to create password" switch on the import wallet key dialog
    And I should see wallet spending password inputs
    And I enter wallet spending password:
    | password  | repeatedPassword |
    | Secret123 | Secret123        |
    And I click on the import wallet button in import wallet dialog
    Then I should not see the import wallet dialog anymore
    And I should have newly created "Genesis wallet" wallet loaded
    And I should be on the "Genesis wallet" wallet "summary" screen
