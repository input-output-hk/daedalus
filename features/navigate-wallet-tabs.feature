Feature: Navigate Wallet Tabs

  Background:
    Given I have selected English language
    And I have a wallet with funds

  Scenario Outline: Switching Between Wallet Tabs
    Given I am on the "Personal Wallet" wallet "<FROM>" screen
    When I click the wallet <TO> button
    Then I should be on the "Personal Wallet" wallet "<TO>" screen

    Examples:
    | FROM    | TO      |
    | summary | send    |
    | summary | receive |
    | send    | summary |
    | send    | receive |
    | receive | summary |
    | receive | send    |
