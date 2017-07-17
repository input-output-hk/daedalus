Feature: Add Wallet via Sidebar

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"
    And I have a wallet with funds

  Scenario: Successfully Restoring a Wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet dialog
    And I click on the restore wallet button in add wallet dialog
    And I see the restore wallet dialog
    And I submit the restore wallet dialog with the following inputs:
    | walletName      | recoveryPhrase                                                            |
    | Restored wallet | marriage glide need gold actress grant judge eager spawn plug sister whip |
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Restored wallet" wallet loaded
    And I should be on the "Restored wallet" wallet "summary" screen

  Scenario: Successfully Restoring a Wallet with spending password
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet dialog
    And I click on the restore wallet button in add wallet dialog
    And I see the restore wallet dialog
    And I toggle "Activate to create password" switch on the restore wallet dialog
    And I submit the restore wallet with spending password dialog with the following inputs:
    | walletName      | password  | repeatedPassword | recoveryPhrase                                                            |
    | Restored wallet | Secret123 | Secret123        | marriage glide need gold actress grant judge eager spawn plug sister whip |
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Restored wallet" wallet loaded
    And I should be on the "Restored wallet" wallet "summary" screen
