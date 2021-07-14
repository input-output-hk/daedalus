@e2e @watch
Feature: Wallet currency display settings

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test Wallet |

  Scenario Outline: User selects alternative currency's to display wallet balance
    Given I am on the General Settings "wallets" screen
    And I open currency selection dropdown
    When I select "<currency>" as the selected displayed currency
    And I am on the "Test Wallet" wallet "summary" screen
    Then I should see "<ticker>" displayed beside wallet balance

    Examples:
      | currency  | ticker |
      | Litecoin  | LTC    |
      | Stellar   | XLM    |

  Scenario: User hits toggle button to show/not show ada wallet balance in other currency's
    Given I am on the General Settings "wallets" screen
    When I toggle the button off to change if I want to see my ada balance in other currency's
    Then the currency selection box is hidden
    And I am on the "Test Wallet" wallet "summary" screen
    Then The wallet summary screen does not show ada balance in other currency's placeholder
    When I am on the General Settings "wallets" screen
    And I toggle the button on to change if I want to see my ada balance in other currency's
    When I am on the General Settings "wallets" screen
    Then the currency selection box is visible
    And I am on the "Test Wallet" wallet "summary" screen
    Then The wallet summary screen does show ada balance in other currency's placeholder
