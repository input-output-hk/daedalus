Feature: Wallet Settings

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"
    And I have the following wallets:
    | name   | password  |
    | first  |           |
    | second | Secret123 |

  Scenario: Wallet password setting
    Given I am on the "first" wallet "settings" screen
    And I click on the "create" password label
    And I should see the "create" wallet password dialog
    And I set wallet password:
    | password | repeatedPassword |
    | Secret123   | Secret123           |
    And I click on the "save" button in "create" wallet password dialog
    Then I should see "change" label in password field

  Scenario: Wallet setting invalid password
    Given I am on the "first" wallet "settings" screen
    And I click on the "create" password label
    And I should see the "create" wallet password dialog
    And I set wallet password:
    | password | repeatedPassword |
    | secret   | secret           |
    And I click on the "save" button in "create" wallet password dialog
    Then I should see the following error messages:
    | message                             |
    | global.errors.invalidWalletPassword |

  Scenario: Wallet password update/change
    Given I am on the "second" wallet "settings" screen
    And I click on the "change" password label
    And I should see the "change" wallet password dialog
    And I change wallet password:
    | currentPassword | password     | repeatedPassword |
    | Secret123       | newSecret123 | newSecret123     |
    And I click on the "save" button in "create" wallet password dialog
    Then I should not see the change password dialog anymore

  Scenario: Wallet password remove
    Given I am on the "second" wallet "settings" screen
    And I click on the "change" password label
    And I should see the "change" wallet password dialog
    And I toggle "Check to deactivate password" switch on the change wallet password dialog
    And I enter current wallet password:
    | currentPassword |
    | Secret123       |
    And I click on the "remove" button in "change" wallet password dialog
    Then I should see "create" label in password field

  Scenario: Wallet rename
    Given I am on the "first" wallet "settings" screen
    And I click on "name" input field
    And I enter new wallet name:
    | name         |
    | first Edited |
    And I click outside form input fields
    Then I should see new wallet name "first Edited"

  Scenario: Wallet assurance level change
    Given I am on the "first" wallet "settings" screen
    And I open Transaction assurance security level selection dropdown
    And I select "Strict" assurance level
    And I click outside form input fields
    Then I should have wallet Strict assurance level