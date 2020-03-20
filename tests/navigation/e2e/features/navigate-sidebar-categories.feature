@e2e
Feature: Navigate Sidebar Categories

  Background:
    Given I have completed the basic setup
    And I have the following "Rewards" wallets:
      | name        |
      | Rewards Wallet |

  Scenario Outline: Navigate Between Sidebar Categories
    Given The sidebar shows the "<FROM>" category
    When I click on the "<TO>" category in the sidebar
    Then The "<TO>" category should be active

    Examples:
    | FROM           | TO             |
    | wallets        | settings       |
    | settings       | wallets        |

  Scenario: Navigate from "Rewards" Wallet to Settings screen
    Given I am on the "Rewards Wallet" wallet "summary" screen
    And The sidebar shows the "wallets" category
    When I click on the "settings" category in the sidebar
    Then I should be on the "settings/general" screen

  Scenario: Open "Rewards" Wallets Menu from Settings Screen
    Given I am on the settings screen
    And The sidebar shows the "settings" category
    When I click on the "wallets" category in the sidebar
    Then The "wallets" category should be active
    But I should be on the "Rewards Wallet" wallet "summary" screen