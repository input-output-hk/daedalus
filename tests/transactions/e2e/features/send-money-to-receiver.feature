@e2e
Feature: Send Money to Receiver

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name   |
      | first  |

  Scenario: User Sends Money to Receiver
    Given I have a "Test Wallet" wallet with funds
    And I am on the "Test Wallet" wallet "send" screen
    And I can see the send form
    When I fill out the send form with a transaction to "first" wallet:
      | amount   |
      | 0.000010 |
    And the transaction fees are calculated
    And I click on the next button in the wallet send form
    And I see send money confirmation dialog
    And I enter wallet spending password in confirmation dialog "Secret1234"
    And I submit the wallet send form
    Then I should be on the "Test Wallet" wallet "summary" screen
    And the latest transaction should show:
      | title                   | amountWithoutFees |
      | wallet.transaction.sent | -0.000010         |
    And the balance of "first" wallet should be:
      | balance |
      | 0.00001 |

  Scenario: User Enters Wrong Receiver Address
    Given I am on the "first" wallet "send" screen
    And I can see the send form
    When I fill out the wallet send form with:
      | address | amount    |
      | invalid | 0.000010  |
    Then I should see the following error messages on the wallet send form:
      | message                   |
      | api.errors.invalidAddress |

  Scenario Outline: User Enters Wrong Amount
    Given I am on the "first" wallet "send" screen
    And I can see the send form
    When I fill out the send form with a transaction to "first" wallet:
      | title          | amount         |
      | my transaction | <WRONG_AMOUNT> |
    Then I should see the following error messages on the wallet send form:
      | message |
      | <ERROR> |

    Examples:
      | WRONG_AMOUNT | ERROR                                        |
      | 99999999     | api.errors.NotEnoughFundsForTransactionError |
      | 0            | wallet.send.form.errors.invalidAmount        |
