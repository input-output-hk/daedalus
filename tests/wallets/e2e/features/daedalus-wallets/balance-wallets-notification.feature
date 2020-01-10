@e2e
Feature: "Daedalus Balance" wallet top bar notification

  Background:
    Given I have completed the basic setup

  Scenario: Daedalus Balance wallet "Create a Rewards wallet" / "Move ada" notification is NOT shown when "Balance" wallet is empty
    When I restore "Daedalus Rewards Wallet" rewards wallet with funds
    Then I should have newly created "Daedalus Rewards Wallet" wallet loaded
    And I restore "Daedalus Balance Wallet" balance wallet without funds
    Then I should have newly created "Daedalus Balance Wallet" wallet loaded
    When I am on the "Daedalus Balance Wallet" wallet "summary" screen
    Then I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet notification should not be displayed in the wallet top bar

  Scenario: Daedalus Balance wallet "Create a Rewards wallet" notification is displayed if the wallet is NOT empty and I don't have a Rewards wallet in the UI
    When I restore "Daedalus Balance Wallet" balance wallet with funds
    Then I should have newly created "Daedalus Balance Wallet" wallet loaded
    And I should be on the "Daedalus Balance Wallet" wallet "summary" screen
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Create a new Rewards wallet" action should be visible in the top bar notification
    When I click "Balance" wallet top bar notification action
    Then I should be on the "wallets/add" screen

  Scenario: Daedalus Balance wallet "Move testnet ada" notification is shown when "Balance" wallet is NOT empty and I have a Rewards wallet in the UI
    When I restore "Daedalus Rewards Wallet" rewards wallet with funds
    Then I should have newly created "Daedalus Rewards Wallet" wallet loaded
    And I restore "Daedalus Balance Wallet" balance wallet with funds
    Then I should have newly created "Daedalus Balance Wallet" wallet loaded
    When I am on the "Daedalus Balance Wallet" wallet "summary" screen
    Then I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification
    When I click "Balance" wallet top bar notification action
    Then I should see "Transfer ada" wizard