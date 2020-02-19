@e2e @watch
Feature: Wallet Delegation

  Background:
    Given I have completed the basic setup

  Scenario: "Delegation screen" current and next epoch countdown are correctly displayed and have correct data
    Given I have the following "Rewards" wallets:
      | name   |
      | Wallet |
    And I am on the Delegation "delegation-center" screen
    Then the current and next epoch countdown are correctly displayed
    And the current and next epoch countdown have correct data

  Scenario: "Delegation center" displays screen until the Stake Pools are loaded
    Given I have a "Wallet Sender" rewards wallet with funds
    And I am on the Delegation "delegation-center" screen
    Then I should see a "Loading stake pools" message until the Stake Pools are loaded

  Scenario: "Create rewards wallet" notification when no Rewards wallets
    Given I am on the Delegation "delegation-center" screen
    Then I should see a "Create rewards wallet" notification

  Scenario: Only Rewards wallets are listed on the "Delegation center" screen
    Given I have the following "Balance" wallets:
      | name            |
      | Legacy Wallet 1 |
      | Legacy Wallet 2 |
    And I have the following "Rewards" wallets:
      | name            |
      | Reward Wallet 1 |
      | Reward Wallet 2 |
    And I am on the Delegation "delegation-center" screen
    Then I should only see Reward wallets listed

  Scenario: "Delegation center" correctly displays undelegated and delegated information and options
    And I have a "First Wallet" rewards wallet with funds
    And I have a "Second Wallet" rewards wallet with funds
    Given I am on the Delegation "delegation-center" screen
    Given the "First Wallet" wallet was delegated to the first Stake Pool
    Then the "First Wallet" wallet should display the delegated Stake Pool ticker
    And I should see the delegated menu with "Change delegation" and "Undelegate" options
    Given I start the wallet delegation process for the "Second Wallet" wallet
    And I click the wallet selector
    Then The "First Wallet" wallet option should display the correct Stake Pool ticker
    And I close the delegation process dialog
    Given the "First Wallet" wallet is undelegated
    Then the "First Wallet" wallet should display the "Delegate" option

  Scenario: "Delegation" wizard displays correct availability
    Given I have the following "Rewards" wallets:
      | name            |
      | Wallet Receiver |
    And I am on the Delegation "delegation-center" screen
    And I start the wallet delegation process for the "Wallet Receiver" wallet
    Then I should see a "Delegation not available" message
    Then I close the wizard
    Given I have a "Wallet Sender" rewards wallet with funds
    And I send 9 ADA from the "Wallet Sender" wallet to the "Wallet Receiver" wallet
    And I start the wallet delegation process for the "Wallet Receiver" wallet
    Then I should see a "This wallet does not contain the minimum amount of 10 ADA which is required for delegation to be available. Please select a wallet with " message
    Given I close the wizard
    And I send 11 ADA from the "Wallet Sender" wallet to the "Wallet Receiver" wallet
    Then I sucessfully delegate my wallet

  Scenario: "Delegation" wizard is showing the correct error message if the user submits wrong spending password
    Given I have the following "Rewards" wallets:
      | name            |
      | Wallet Receiver |
    And I have a "Wallet Sender" rewards wallet with funds
    And I am on the Delegation "delegation-center" screen
    And I start the wallet delegation process for the "Wallet Receiver" wallet
    And I choose the "Wallet Sender" wallet
    And I choose the first stake pool
    And I enter "INCORRECT" as the spending password
    Then I should see a "Incorrect wallet password." message

  @skip
  Scenario Outline: Pending delegations
    Given I have completed the basic setup
    And I have the following "Rewards" wallets:
      | name              |
      | Unmodified Wallet |
    And I am on the Delegation "delegation-center" screen
    And I mark experimental feature as read
    Given the wallet has the following <DELEGATION_SCENARIO>
    Then the wallet should correctly display the correct stake pool tickers
    And the <ACTIVE_POOL> should display the ADA logo
    And the tickers should display the correct <TOOLTIPS>
    And the wallet should display the correct <LINKS>

    Examples:
    | DELEGATION_SCENARIO                   | ACTIVE_POOL | TOOLTIPS | LINKS                    |
    | undelegated                           | none        | none     | Delegate                 |
    | undelegated > delegated               | none        | none     | Undelegate or Redelegate |
    | undelegated > delegated > undelegated | none        | none     | Delegate                 |
    | undelegated > delegated > delegated   | none        | none     | Undelegate or Redelegate |
    | delegated                             | none        | none     | Undelegate or Redelegate |
    | delegated > undelegated               | none        | none     | Delegate                 |
    | delegated > delegated                 | none        | none     | Undelegate or Redelegate |
    | delegated > delegated > undelegated   | none        | none     | Delegate                 |
    | delegated > delegated > delegated     | none        | none     | Undelegate or Redelegate |


  @skip
  # We are currently not displaying stake pools with no metadata
  # And have no Unknown state for delegated wallets
  Scenario: "Unknown" stake pool is shown for the wallets being delegated to stake pools for which we don't have metadata
    And I have a "Wallet - No Metadata" rewards wallet with funds
    And the "Wallet - No Metadata" wallet was delegated to a Stake Pool with no metadata
    And I am on the Delegation "delegation-center" screen
    Then I should see a "Unknown" stake pool ticker for the delegated wallet
