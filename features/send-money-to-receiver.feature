Feature: Send Money to Receiver

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"
    And I have the following wallets:
      | name   |
      | first  |

  Scenario: User Sends Money to Receiver
    Given I have a wallet with funds
    And I am on the "Genesis wallet" wallet "send" screen
    When I fill out the send form with a transaction to "first" wallet:
      | amount   |
      | 0.000010 |
    And I submit the wallet send form
    Then I should be on the "Genesis wallet" wallet "summary" screen
    And the latest transaction should show:
      | title                      | amount    |
      | wallet.transaction.adaSent | -0.000010 |
    And the balance of "first" wallet should be:
      | balance  |
      | 0.000010 |

  Scenario: User Sends Money from wallet with spending password to Receiver
    Given I have a wallet with funds and password
    And I am on the "Genesis wallet" wallet "send" screen
    When I fill out the send form with a transaction to "first" wallet:
      | amount   |  walletPassword |
      | 0.000010 |  Secret123      |
    And I submit the wallet send form
    Then I should be on the "Genesis wallet" wallet "summary" screen
    And the latest transaction should show:
      | title                      | amount    |
      | wallet.transaction.adaSent | -0.000010 |
    And the balance of "first" wallet should be:
      | balance  |
      | 0.000010 |

  Scenario: User Submits Empty Form
    Given I have a wallet with funds
    And I am on the "Genesis wallet" wallet "send" screen
    When I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                       |
      | global.errors.fieldIsRequired |
      | global.errors.fieldIsRequired |

  Scenario: User Enters Wrong Receiver Address
    Given I have a wallet with funds
    And I am on the "Genesis wallet" wallet "send" screen
    When I fill out the wallet send form with:
      | address | amount    |
      | invalid | 0.000010  |
    And I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                                |
      | wallet.send.form.errors.invalidAddress |

  Scenario Outline: User Enters Wrong Amount
    Given I have a wallet with funds
    And I am on the "Genesis wallet" wallet "send" screen
    When I fill out the send form with a transaction to "first" wallet:
      | title          | amount         |
      | my transaction | <WRONG_AMOUNT> |
    And I submit the wallet send form
    Then I should see the following error messages on the wallet send form:
      | message                               |
      | wallet.send.form.errors.invalidAmount |

    Examples:
      | WRONG_AMOUNT |
      | 4500000001   |
      | 0            |
