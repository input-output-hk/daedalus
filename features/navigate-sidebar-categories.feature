@skip
Feature: Navigate Sidebar Categories

  Background:
    Given I have a wallet

  Scenario Outline: Switching Sidebar Categories
    Given The sidebar shows the <FROM> category
    When I click on the <TO> category in the sidebar
    Then The <TO> category should be active

    Examples:
    | FROM           | TO             |
    | wallets        | ada-redemption |
    | ada-redemption | wallets        |

  Scenario: Navigate from a Wallet to Ada Redemption screen
    Given I am on the "Personal Wallet" wallet "summary" screen
    And The sidebar shows the wallets category
    When I click on the ada-redemption category in the sidebar
    Then I should be on the ada redemption screen

  Scenario: Open Wallets Menu from Ada Redemption Screen
    Given I am on the ada redemption screen
    And The sidebar shows the ada-redemption category
    When I click on the wallets category in the sidebar
    Then The wallets category should be active
    But I should be on the wallet summary screen
