Feature: Navigate Sidebar Categories

  Background:
    Given I have a wallet

  Scenario Outline: Switching Sidebar Categories
    Given The sidebar shows the <FROM> category
    When I click on the <TO> category in the sidebar
    Then The <TO> category should be active

    Examples:
    | FROM     | TO       |
    | wallets  | settings |
    | settings | wallets  |

  Scenario: Navigate from a Wallet to Profile Settings
    Given I am on the wallet home screen
    And The sidebar shows the wallets category
    When I click on the settings category in the sidebar
    Then I should be on the profile settings screen

  Scenario: Open Wallets Menu from Settings Screen
    Given I am on the settings screen
    And The sidebar shows the settings category
    When I click on the wallets category in the sidebar
    Then The wallets category should be active
    But I should still be on the profile settings screen
