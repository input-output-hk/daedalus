@reset @skip
Feature: Send Money to Receiver

  Background:
    Given I have the following wallets:
      | name   |
      | first  |

  Scenario: User Sends Money to Receiver
    Given I am on the "Personal Wallet" wallet "send" screen
    When I fill out the send form with a transaction to "first" wallet:
      | title          | amount | description |
      | my transaction | 10     | some text   |
    And I submit the wallet send form
    Then I should see the "Personal Wallet" wallet home screen with the transaction titled "my transaction"

  Scenario: User Submits Empty Form
    Given I am on the "Personal Wallet" wallet "send" screen
    When I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                               |
      | wallet.send.form.errors.invalidTitle  |
      | global.errors.fieldIsRequired         |
      | wallet.send.form.errors.invalidAmount |

  Scenario: User Enters Wrong Receiver Address
    Given I am on the "Personal Wallet" wallet "send" screen
    When I fill out the wallet send form with:
      | title          | address | amount | description |
      | my transaction | invalid | 10     | some text   |
    And I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                                |
      | wallet.send.form.errors.invalidAddress |

  Scenario Outline: User Enters Wrong Amount
    Given I am on the "Personal Wallet" wallet "send" screen
    When I fill out the send form with a transaction to "first" wallet:
      | title          | amount         | description |
      | my transaction | <WRONG_AMOUNT> | some text   |
    And I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                               |
      | wallet.send.form.errors.invalidAmount |

    Examples:
      | WRONG_AMOUNT |
      | -15          |
      | 5,5          |
      | 5.5          |
      | text         |
