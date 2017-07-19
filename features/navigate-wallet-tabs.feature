Feature: Navigate Wallet Tabs

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"
    And I agree to send logs to remote server
    And I have the following wallets:
      | name        |
      | Test wallet |

  Scenario Outline: Switching Between Wallet Tabs
    Given I am on the "Test wallet" wallet "<FROM>" screen
    When I click the wallet <TO> button
    Then I should be on the "Test wallet" wallet "<TO>" screen

    Examples:
    | FROM    | TO      |
    | summary | send    |
    | summary | receive |
    | send    | summary |
    | send    | receive |
    | receive | summary |
    | receive | send    |
