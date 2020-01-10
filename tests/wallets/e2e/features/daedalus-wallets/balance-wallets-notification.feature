@e2e
Feature: "Daedalus Balance" wallet notification

  Background:
    Given I have completed the basic setup

  Scenario: Daedalus Balance wallet "Create a Rewards wallet" / "Move ada" notification is NOT shown when "Balance" wallet is empty
    When I restore "Daedalus Balance wallet" balance wallet without funds
    And I restore "Rewards Wallet" wallet with funds
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Daedalus Balance wallet" wallet loaded
    And "Daedalus Balance wallet" wallet should have "legacy_575aa8238ee09ea2ac55d2990247f5663db4ca09" as id
    And I should be on the "Daedalus Balance wallet" wallet "summary" screen
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet notification should not be displayed in the wallet top bar

  Scenario: Daedalus Balance wallet "Create a Rewards wallet" notification is displayed if the wallet is NOT empty and I don't have a Rewards wallet in the UI
    When I restore "Daedalus Balance wallet" balance wallet with funds
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Daedalus Balance wallet" wallet loaded
    And "Daedalus Balance wallet" wallet should have "legacy_0b3a38e4078206ccd93cc353a93cc3a37dbbb4fe" as id
    And I should be on the "Daedalus Balance wallet" wallet "summary" screen
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Create a new Rewards wallet" action should be visible in the top bar notification
    And I click "Balance" wallet top bar notification action
    Then I should be on the "wallets/add" screen

  Scenario: Daedalus Balance wallet "Move testnet ada" notification is shown when "Balance" wallet is NOT empty and I have a Rewards wallet in the UI
    And I restore "Daedalus Balance wallet" balance wallet with funds
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Daedalus Balance wallet" wallet loaded
    And "Daedalus Balance wallet" wallet should have "legacy_0b3a38e4078206ccd93cc353a93cc3a37dbbb4fe" as id
    And I should be on the "Daedalus Balance wallet" wallet "summary" screen
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And I restore "Rewards Wallet" wallet with funds
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification
    And I click "Balance" wallet top bar notification action
    Then I should see "Transfer ada" wizard