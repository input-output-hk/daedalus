Feature: Wallet Settings

  Background:
    Given I have completed the basic setup
    And I create wallets until I reach the maximum number permitted

  Scenario: User reaches the maximum number of wallets
    Given I click on the add wallet button in the sidebar
    Then I should see maximum number of wallets in the wallets list
    And The buttons in the Add Wallet screen should be disabled
    And I should see a disclaimer saying I have reached the maximum number of wallets

  Scenario: User deletes one wallet and re-enable its Adding new wallets
    Given I am on the "Wallet 20" wallet "settings" screen
    When I click on delete wallet button
    And I see delete wallet dialog
    And I click on the "Make sure you have access to backup before continuing" checkbox
    And I enter "Wallet 20" as name of the wallet to confirm
    And I submit the delete wallet dialog
    And I click on the add wallet button in the sidebar
    Then The buttons in the Add Wallet screen should be enabled
