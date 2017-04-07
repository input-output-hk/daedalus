Feature: Send Money to Receiver

  Background:
    Given I have selected English language
    And I have a wallet with funds
    And I have the following wallets:
      | name   |
      | first  |

  Scenario: User Sends Money to Receiver
    Given I am on the "Personal Wallet" wallet "send" screen
    When I fill out the send form with a transaction to "first" wallet:
      | amount |
      | 10     |
    And I submit the wallet send form
    Then I should be on the "Personal Wallet" wallet "summary" screen
    And the latest transaction should show:
      | title    | amount |
      | Ada Sent | -10    |

  Scenario: User Submits Empty Form
    Given I am on the "Personal Wallet" wallet "send" screen
    When I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                               |
      | global.errors.fieldIsRequired         |
      | wallet.send.form.errors.invalidAmount |

  Scenario: User Enters Wrong Receiver Address
    Given I am on the "Personal Wallet" wallet "send" screen
    When I fill out the wallet send form with:
      | address | amount |
      | invalid | 10     |
    And I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                                |
      | wallet.send.form.errors.invalidAddress |

  Scenario Outline: User Enters Wrong Amount
    Given I am on the "Personal Wallet" wallet "send" screen
    When I fill out the send form with a transaction to "first" wallet:
      | title          | amount         |
      | my transaction | <WRONG_AMOUNT> |
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
