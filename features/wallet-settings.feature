@e2e
Feature: Wallet Settings

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name   | password   |
      | first  |            |
      | second | Secret1234 |

  # It is not possible to set wallet password because it is always available and required in API v2
  @skip
  Scenario: User sets Wallet password
    Given I am on the "first" wallet "settings" screen
    And I click on the "create" password label
    And I should see the "create" wallet password dialog
    And I enter wallet password:
      | password   | repeatedPassword |
      | Secret1234 | Secret1234       |
    And I submit the wallet password dialog
    Then I should see "change" label in password field

  # It is not possible to set wallet password because it is always available and required in API v2
  @skip
  Scenario: User tries to set Wallet password with invalid password format
    Given I am on the "first" wallet "settings" screen
    And I click on the "create" password label
    And I should see the "create" wallet password dialog
    And I enter wallet password:
      | password | repeatedPassword |
      | secret   | secret           |
    And I submit the wallet password dialog
    Then I should see the following error messages:
      | message                               |
      | global.errors.invalidSpendingPassword |

  Scenario: User changes Wallet password
    Given I am on the "second" wallet "settings" screen
    And I click on the "change" password label
    And I should see the "change" wallet password dialog
    And I change wallet password:
      | currentPassword | password      | repeatedPassword |
      | Secret1234      | newSecret1234 | newSecret1234    |
    And I submit the wallet password dialog
    Then I should not see the change password dialog anymore

  Scenario: User tries to change Wallet password with wrong old password
    Given I am on the "second" wallet "settings" screen
    And I click on the "change" password label
    And I should see the "change" wallet password dialog
    And I change wallet password:
      | currentPassword  | password      | repeatedPassword |
      | Secret1234Wrong  | newSecret1234 | newSecret1234    |
    And I submit the wallet password dialog
    Then I should see error message that old password is not correct

  Scenario: User changes wallet password to one which contains only cyrillic characters and numbers
    Given I am on the "second" wallet "settings" screen
    And I click on the "change" password label
    And I should see the "change" wallet password dialog
    And I change wallet password:
      | currentPassword | password           | repeatedPassword   |
      | Secret1234      | ЬнЫгзукЗфыыцщкв123 | ЬнЫгзукЗфыыцщкв123 |
    And I submit the wallet password dialog
    Then I should not see the change password dialog anymore

  Scenario: User changes wallet password to one which contains only japanese characters and numbers
    Given I am on the "second" wallet "settings" screen
    And I click on the "change" password label
    And I should see the "change" wallet password dialog
    And I change wallet password:
      | currentPassword | password     | repeatedPassword |
      | Secret1234      | 新しい秘密123  | 新しい秘密123     |
    And I submit the wallet password dialog
    Then I should not see the change password dialog anymore

  # It is not possible to remove wallet password because it is required in API v2
  @skip
  Scenario: User removes Wallet password
    Given I am on the "second" wallet "settings" screen
    And I click on the "change" password label
    And I should see the "change" wallet password dialog
    And I toggle "Check to deactivate password" switch on the change wallet password dialog
    And I enter current wallet password:
      | currentPassword  |
      | Secret1234       |
    And I submit the wallet password dialog
    Then I should see "create" label in password field

  Scenario: User renames Wallet
    Given I am on the "first" wallet "settings" screen
    And I click on "name" input field
    And I enter new wallet name:
      | name         |
      | first Edited |
    And I click outside "name" input field
    Then I should see new wallet name "first Edited"

  Scenario: User renames Wallet to a name which includes non-latin characters
    Given I am on the "first" wallet "settings" screen
    And I click on "name" input field
    And I enter new wallet name:
      | name     |
      | キュビズム |
    And I click outside "name" input field
    Then I should see new wallet name "キュビズム"

  # It is not possible to change wallet assurance level because it is not available in API v2
  @skip
  Scenario: User changes Wallet assurance level
    Given I am on the "first" wallet "settings" screen
    And I open "Transaction assurance security level" selection dropdown
    And I select "Strict" assurance level
    Then I should have wallet with "Strict" assurance level set
