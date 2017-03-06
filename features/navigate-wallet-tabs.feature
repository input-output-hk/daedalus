Feature: Navigate Wallet Tabs

  Background:
    Given I have a wallet

  Scenario Outline: Switching Between Wallet Tabs
    Given I am on the "Personal Wallet" wallet "<FROM>" screen
    When I click the wallet <TO> button
    Then I should be on the wallet <TO> screen

    Examples:
    | FROM    | TO      |
    | summary | send    |
    | summary | receive |
    | send    | summary |
    | send    | receive |
    | receive | summary |
    | receive | send    |
