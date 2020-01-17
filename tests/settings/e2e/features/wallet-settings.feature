@e2e
Feature: Wallet Settings

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name   |
      | first  |
      | second |

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

  Scenario: User tries to change Wallet password with invalid password format
    Given I am on the "first" wallet "settings" screen
    And I click on the "change" password label
    And I should see the "change" wallet password dialog
    And I change wallet password:
      | currentPassword | password      | repeatedPassword |
      | Secret1234      | secret        | secret.          |
    And I submit the wallet password dialog
    Then I should see the following error messages:
      | message                               |
      | global.errors.invalidSpendingPassword |

  Scenario: User changes wallet password to one which contains only cyrillic characters and numbers
    Given I am on the "second" wallet "settings" screen
    And I click on the "change" password label
    And I should see the "change" wallet password dialog
    And I change wallet password:
      | currentPassword | password            | repeatedPassword    |
      | Secret1234      | ЬнЫгзукЗфыыцщкв1234 | ЬнЫгзукЗфыыцщкв1234 |
    And I submit the wallet password dialog
    Then I should not see the change password dialog anymore

  Scenario: User changes wallet password to one which contains only japanese characters and numbers
    Given I am on the "second" wallet "settings" screen
    And I click on the "change" password label
    And I should see the "change" wallet password dialog
    And I change wallet password:
      | currentPassword | password       | repeatedPassword |
      | Secret1234      | 新しい秘密12345  | 新しい秘密12345     |
    And I submit the wallet password dialog
    Then I should not see the change password dialog anymore

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

  Scenario: User force Rewards Wallet resync
    Given I am on the "first" wallet "settings" screen
    When I click "Resync wallet" button
    And I see "Resync wallet" button spinner
    Then I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And I should not see "Resync wallet" button spinner anymore

  Scenario: User force Balance Wallet resync
    Given I restore "Test Balance Wallet" balance wallet with funds
    And I am on the "Test Balance Wallet" wallet "settings" screen
    And I should not see the restore status notification once restore is finished
    When I click "Resync wallet" button
    And I see "Resync wallet" button spinner
    Then I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And I should not see "Resync wallet" button spinner anymore
