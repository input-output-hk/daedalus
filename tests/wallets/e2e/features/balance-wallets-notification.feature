@e2e @wip
Feature: "Daedalus Byron" wallet top bar notification

  Background:
    Given I have completed the basic setup

  Scenario: Byron wallet "Create a Shelley wallet" / "Move ada" notification is NOT shown when "Byron" wallet is empty
    Given I have the following wallets:
      | name           |
      | Shelley Wallet |
    And I have a "Byron Wallet" byron wallet
    Then I should have newly created "Byron Wallet" wallet loaded
    When I am on the "Byron Wallet" wallet "summary" screen
    Then I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Byron" wallet badge should be visible in the wallet sidebar
    And "Byron" wallet notification should not be displayed in the wallet top bar

  Scenario: Byron wallet "Create a Shelley wallet" notification is displayed if the wallet is NOT empty and I don't have a Shelley wallet in the UI
    Given I have a "Byron Wallet" byron wallet with funds
    Then I should have newly created "Byron Wallet" wallet loaded
    And I should be on the "Byron Wallet" wallet "summary" screen
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Byron" wallet badge should be visible in the wallet sidebar
    And "Byron" wallet "Create a new Shelley wallet" action should be visible in the top bar notification
    When I click "Byron" wallet top bar notification action
    Then I should be on the "wallets/add" screen

  Scenario: Byron wallet "Move testnet ada" notification is shown when "Byron" wallet is NOT empty and I have a Shelley wallet in the UI
    Given I have the following wallets:
      | name           |
      | Shelley Wallet |
    And I have a "Byron Wallet" byron wallet with funds
    Then I should have newly created "Byron Wallet" wallet loaded
    When I am on the "Byron Wallet" wallet "summary" screen
    Then I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Byron" wallet badge should be visible in the wallet sidebar
    And "Byron" wallet "Move testnet ada" action should be visible in the top bar notification
    When I click "Byron" wallet top bar notification action
    Then I should see "Transfer ada" wizard
