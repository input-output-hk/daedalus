@e2e
Feature: Navigate Wallet Tabs

  Background:
    Given I have completed the basic setup

  Scenario Outline: Switching Between "Rewards" Wallet Tabs
    Given I have the following "Rewards" wallets:
      | name        |
      | Rewards wallet |
    And I am on the "Rewards wallet" wallet "<FROM>" screen
    When I click the wallet <TO> button
    Then I should be on the "Rewards wallet" wallet "<TO>" screen

    Examples:
    | FROM    | TO           |
    | summary | send         |
    | summary | receive      |
    | summary | transactions |
    | send    | summary      |
    | send    | receive      |
    | send | transactions    |
    | receive | summary      |
    | receive | send         |
    | receive | transactions |
