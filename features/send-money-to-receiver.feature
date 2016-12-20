Feature: Send Money to Receiver

  Background:
    Given I have an account
    And I have a wallet
    And I am logged in

  Scenario: User Sends Money to Receiver
    Given I am on the wallet send screen
    When I fill out the wallet send form with:
      | receiver                           | amount | description |
      | 13GvjwDkz8s8ZmGQjwVLNUXrNXdSmQa72x | 10.90  | some text   |
    And I submit the wallet send form
    Then I should see the wallet home screen with the transaction

  Scenario: User Submits Empty Form
    Given I am on the wallet send screen
    When I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                                       |
      | wallet.send.form.errors.invalidBitcoinAddress |
      | wallet.send.form.errors.invalidAmount         |

  Scenario: User Enters Wrong Receiver Address
    Given I am on the wallet send screen
    When I fill out the wallet send form with:
      | receiver      | amount | description |
      | wrong-address | 10.90  | some text   |
    And I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                                       |
      | wallet.send.form.errors.invalidBitcoinAddress |

  Scenario Outline: User Enters Wrong Amount
    Given I am on the wallet send screen
    When I fill out the wallet send form with:
      | receiver                           | amount         | description |
      | 13GvjwDkz8s8ZmGQjwVLNUXrNXdSmQa72x | <WRONG_AMOUNT> | some text   |
    And I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                               |
      | wallet.send.form.errors.invalidAmount |

    Examples:
      | WRONG_AMOUNT |
      | -15          |
      | 5,5          |
      | text         |
