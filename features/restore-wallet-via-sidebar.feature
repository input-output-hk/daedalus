Feature: Add Wallet via Sidebar

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |

  Scenario: Successfully Restoring a Wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet dialog
    And I click on the restore wallet button in add wallet dialog
    And I see the restore wallet dialog
    And I enter wallet name "Restored wallet" in restore wallet dialog
    And I enter recovery phrase in restore wallet dialog:
    | recoveryPhrase                                                            |
    | marriage glide need gold actress grant judge eager spawn plug sister whip |
    And I submit the restore wallet dialog
    Then I should see the restore status notification while restore is running
    And I should not see the restore wallet dialog anymore
    And I should not see the restore status notification once restore is finished
    And I should have newly created "Restored wallet" wallet loaded

  Scenario: Successfully Restoring a Wallet with spending password
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet dialog
    And I click on the restore wallet button in add wallet dialog
    And I see the restore wallet dialog
    And I enter wallet name "Restored wallet" in restore wallet dialog
    And I enter recovery phrase in restore wallet dialog:
    | recoveryPhrase                                                            |
    | marriage glide need gold actress grant judge eager spawn plug sister whip |
    And I toggle "Activate to create password" switch on the restore wallet dialog
    And I enter wallet password in restore wallet dialog:
    | password  | repeatedPassword |
    | Secret123 | Secret123        |
    And I submit the restore wallet dialog
    Then I should see the restore status notification while restore is running
    And I should not see the restore wallet dialog anymore
    And I should not see the restore status notification once restore is finished
    And I should have newly created "Restored wallet" wallet loaded
