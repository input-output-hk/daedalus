@e2e @byron @skip
Feature: Import wallets

  Background:
    Given I have completed the basic setup

  Scenario: Successfully importing wallets from state directory
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallets overlay
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.stateFolderLabel |
    And I should see Daedalus State directory as predefined import path
    And I set import path
    And I click "Import wallets" button
    And I should see wallets properly listed
    And I should see that all wallets are available for import
    And "Import selected wallets" button is disabled
    And I select all named wallets for import
    And "Import selected wallets" button is enabled
    And I click "Import selected wallets" button and wait until operation is finished
    And "Import selected wallets" button is disabled
    Then I should see that all named wallets are imported
    And I close import wallets dialog by clicking on "Close window" button
    Then I should not see the import wallet dialog anymore
    And The sidebar shows the "wallets" category
    And I should see all imported wallets in left sidebar
    And I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallets overlay
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.stateFolderLabel |
    And I should see Daedalus State directory as predefined import path
    And I set import path
    And I click "Import wallets" button
    And I should see wallets properly listed
    And I should see that all named already exists
    And "Import selected wallets" button is disabled
    And I enter random names to all unnamed wallets
    And I select all unnamed wallets for import
    And "Import selected wallets" button is enabled
    And I click "Import selected wallets" button and wait until operation is finished
    And "Import selected wallets" button is disabled
    Then I should see that all unnamed wallets are imported
    And I close import wallets dialog by clicking on "X" button
    Then I should not see the import wallet dialog anymore
    And The sidebar shows the "wallets" category
    Then I should see all imported wallets in left sidebar

  Scenario: Wrong import wallets state directory path
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallets overlay
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.stateFolderLabel |
    And I should see Daedalus State directory as predefined import path
    And I set wrong import path
    And I click "Import wallets" button
    Then I should see import file selection error message:
      | message                                       |
      | wallet.import.file.dialog.stateDirNoWallets   |

  Scenario: Successfully importing wallets from secret key
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallets overlay
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.stateFolderLabel |
    And I should see Daedalus State directory as predefined import path
    And I select import from secret key
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.secretFileLabel  |
    And I set import secret key path
    And I click "Import wallets" button
    And I should see wallets properly listed
    And I should see that all wallets are available for import
    And "Import selected wallets" button is disabled
    And I select all named wallets for import
    And "Import selected wallets" button is enabled
    And I click "Import selected wallets" button and wait until operation is finished
    And "Import selected wallets" button is disabled
    Then I should see that all named wallets are imported
    And I close import wallets dialog by clicking on "Close window" button
    Then I should not see the import wallet dialog anymore
    And The sidebar shows the "wallets" category
    And I should see all imported wallets in left sidebar
    And I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallets overlay
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.stateFolderLabel |
    And I should see Daedalus State directory as predefined import path
    And I set import secret key path
    And I click "Import wallets" button
    And I should see wallets properly listed
    And I should see that all named already exists
    And "Import selected wallets" button is disabled
    And I enter random names to all unnamed wallets
    And I select all unnamed wallets for import
    And "Import selected wallets" button is enabled
    And I click "Import selected wallets" button and wait until operation is finished
    And "Import selected wallets" button is disabled
    Then I should see that all unnamed wallets are imported
    And I close import wallets dialog by clicking on "X" button
    Then I should not see the import wallet dialog anymore
    And The sidebar shows the "wallets" category
    Then I should see all imported wallets in left sidebar

  Scenario: Wrong import wallets secret key
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallets overlay
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.stateFolderLabel |
    And I should see Daedalus State directory as predefined import path
    And I select import from secret key
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.secretFileLabel  |
    And I set wrong import secret key path
    And I click "Import wallets" button
    Then I should see import file selection error message:
      | message                                       |
      | wallet.import.file.dialog.secretFileNoWallets |

  Scenario: Successfully importing edited wallet
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallets overlay
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.stateFolderLabel |
    And I should see Daedalus State directory as predefined import path
    And I set import path
    And I click "Import wallets" button
    And I should see wallets properly listed
    And I should see that all wallets are available for import
    And "Import selected wallets" button is disabled
    And I edit first wallet name
    And I select all named wallets for import
    And "Import selected wallets" button is enabled
    And I click "Import selected wallets" button and wait until operation is finished
    And "Import selected wallets" button is disabled
    Then I should see that all named wallets are imported
    And I close import wallets dialog by clicking on "Close window" button
    Then I should not see the import wallet dialog anymore
    And The sidebar shows the "wallets" category
    And I should see all imported wallets in left sidebar

  Scenario: Successfully migrating wallets from state directory
    Given I set import path
    And I have wallet migration enabled
    And I should see wallet select import dialog
    And I should see wallets properly listed
    And I should see that all wallets are available for import
    And "Import selected wallets" button is disabled
    And I select all named wallets for import
    And "Import selected wallets" button is enabled
    And I click "Import selected wallets" button and wait until operation is finished
    And "Import selected wallets" button is disabled
    Then I should see that all named wallets are imported
    And I close import wallets dialog by clicking on "Close window" button
    Then I should not see the import wallet dialog anymore
    And The sidebar shows the "wallets" category
    And I should see all imported wallets in left sidebar
    And I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallets overlay
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.stateFolderLabel |
    And I should see Daedalus State directory as predefined import path
    And I set import path
    And I click "Import wallets" button
    And I should see wallets properly listed
    And I should see that all named already exists
    And "Import selected wallets" button is disabled
    And I enter random names to all unnamed wallets
    And I select all unnamed wallets for import
    And "Import selected wallets" button is enabled
    And I click "Import selected wallets" button and wait until operation is finished
    And "Import selected wallets" button is disabled
    Then I should see that all unnamed wallets are imported
    And I close import wallets dialog by clicking on "X" button
    Then I should not see the import wallet dialog anymore
    And The sidebar shows the "wallets" category
    Then I should see all imported wallets in left sidebar

  Scenario: Wrong wallets migration state directory path
    Given I set wrong import path
    And I have wallet migration enabled
    And I should not see wallet select import dialog
    And I see the add wallet page

  Scenario: Wallets migration while app reconnects
    Given I set import path
    And I have wallet migration enabled
    And I should see wallet select import dialog
    And I should see wallets properly listed
    And I should see that all wallets are available for import
    And I disconnect app
    And I should not see wallet select import dialog
    And I see the add wallet page

  Scenario: Importing more than maximum number of wallets is prevented
    Given I have 29 restored wallets
    And I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the import wallet button on the add wallet page
    And I see the import wallets overlay
    And I should see import selection label:
      | label                                      |
      | wallet.import.file.dialog.stateFolderLabel |
    And I should see Daedalus State directory as predefined import path
    And I set import path
    And I click "Import wallets" button
    And I should see wallets properly listed
    And I should see that all wallets are available for import
    And "Import selected wallets" button is disabled
    And I select wallet with index 0 for import
    And "Import selected wallets" button is enabled
    And I hover import selection checkbox for wallet with index 1
    Then Import selection checkbox for wallet with index 1 is disabled
    And I should see maximum wallets reached tooltip for wallet with index 1
    And I click "Import selected wallets" button and wait until operation is finished
    And "Import selected wallets" button is disabled
    And I close import wallets dialog by clicking on "Close window" button
    Then I should not see the import wallet dialog anymore
    And The sidebar shows the "wallets" category
    Then I should see maximum number of wallets in the wallets list
    And the buttons in the Add Wallet screen should be disabled
    And I should see a disclaimer saying I have reached the maximum number of wallets
