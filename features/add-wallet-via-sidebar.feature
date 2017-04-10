Feature: Add Wallet via Sidebar

  Background:
    Given I have a wallet with funds

  Scenario: Successfully Adding a Wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet dialog
    And I click on the create wallet button in add wallet dialog
    And I see the create wallet dialog
    And I submit the create wallet dialog with the following inputs:
    | walletName |
    | Test       |
    And I see the create wallet privacy dialog
    And I click on "Please make sure nobody looks your screen" checkbox
    And I wait for 10 seconds and submit the create wallet privacy dialog
    And I see the create wallet recovery phrase display dialog
    And I submit the create wallet recovery phrase display dialog
    And I see the create wallet recovery phrase entry dialog
    Then I should be on the "Test" wallet "summary" screen
    And I dont see the create wallet dialog anymore
