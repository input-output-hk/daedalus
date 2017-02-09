Feature: Navigate Wallet Tabs

  Background:
    Given I have a wallet

  Scenario Outline: Switching Between Wallet Tabs
    Given I am on the wallet <FROM> screen
    When I click the wallet <TO> button
    Then I should be on the wallet <TO> screen

    Examples:
    | FROM    | TO      |
    | home    | send    |
    | home    | receive |
    | send    | home    |
    | send    | receive |
    | receive | home    |
    | receive | send    |
