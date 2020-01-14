@e2e
Feature: Add Wallet via Sidebar

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test Wallet |

  Scenario: Successfully Adding a Wallet with spending password
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the create wallet button on the add wallet page
    And I see the create wallet dialog
    And I submit the create wallet with spending password dialog with the following inputs:
      | walletName | password  | repeatedPassword |
      | New wallet | Secret1234 | Secret1234      |
    And I see the create wallet privacy dialog
    And I click on "Please make sure nobody looks your screen" checkbox
    And I submit the create wallet privacy dialog
    And I see the create wallet recovery phrase display dialog
    And I note down the recovery phrase
    And I submit the create wallet recovery phrase display dialog
    And I see the create wallet recovery phrase entry dialog
    And I click on recovery phrase mnemonics in correct order
    And I click on the "Accept terms" checkboxes
    And I submit the create wallet recovery phrase entry dialog
    Then I should not see the create wallet recovery phrase entry dialog anymore
    And I should have newly created "New wallet" wallet loaded
    And I should be on the "New wallet" wallet "summary" screen
