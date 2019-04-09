@e2e
Feature: Navigate Wallet Tabs

  Background:
    Given I have completed the basic setup
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
