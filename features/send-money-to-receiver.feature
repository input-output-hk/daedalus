Feature: Send Money to Receiver

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"
    And I have a wallet with funds
    And I have the following wallets:
      | name   |
      | first  |

  Scenario: User Sends Money to Receiver
    Given I am on the "Genesis wallet" wallet "send" screen
    When I fill out the send form with a transaction to "first" wallet:
      | amount   |
      | 0.000010 |
    And I submit the wallet send form
    Then I should be on the "Genesis wallet" wallet "summary" screen
    And the latest transaction should show:
      | title                      | amount    |
      | wallet.transaction.adaSent | -0.000010 |

  Scenario: User Submits Empty Form
    Given I am on the "Genesis wallet" wallet "send" screen
    When I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                               |
      | global.errors.fieldIsRequired         |
      | wallet.send.form.errors.invalidAmount |

  Scenario: User Enters Wrong Receiver Address
    Given I am on the "Genesis wallet" wallet "send" screen
    When I fill out the wallet send form with:
      | address | amount    |
      | invalid | 0.000010  |
    And I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                                |
      | wallet.send.form.errors.invalidAddress |

  @skip
  Scenario Outline: User Enters Wrong Amount
    Given I am on the "Genesis wallet" wallet "send" screen
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
