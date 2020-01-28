@e2e
Feature: Wallet Delegation

  Background:
    Given I have completed the basic setup

  # Scenario: "Loading stake pools" message is shown on the "Delegation center" screen until we load Stake pools

  Scenario: "Create rewards wallet" notification when no Rewards wallets
    Given I am on the Delegation "delegation-center" screen
    Then I should see a "Create rewards wallet" notification

  Scenario: Only Rewards wallets are listed on the "Delegation center" screen
    Given I have the following legacy wallets:
      | name            |
      | Legacy Wallet 1 |
      | Legacy Wallet 2 |
    And I have the following wallets:
      | name            |
      | Reward Wallet 1 |
      | Reward Wallet 2 |
    And I am on the Delegation "delegation-center" screen
    Then I should only see Reward wallets listed

  Scenario: "Delegation screen" current and next epoch countdown are correctly displayed and have correct data
    Given I have the following wallets:
      | name   |
      | Wallet |
    And I am on the Delegation "delegation-center" screen
    Then the current and next epoch countdown are correctly displayed
    # TODO:
    # And the current and next epoch countdown have correct data

  Scenario: "Delegation center" correctly displays undelegated and delegated information and options
    Given I have a "Test Wallet" wallet with funds
    And I am on the Delegation "delegation-center" screen
    Then I should see the "delegate" option
    Given My wallet was delegated
    Then I should see the delegated pool name
    And I should see the delegated menu with "Change delegation" and "Undelegate" options
    Then I freeze

  # Scenario: "Unknown" stake pool is shown for the wallets being delegated to stake pools for which we don't have metadata

  Scenario: "Delegation" wizard displays correct availability
    Given I have the following wallets:
      | name            |
      | Wallet Receiver |
    And I am on the Delegation "delegation-center" screen
    And I start the wallet delegation process
    Then I should see a "Delegation not available" message
    Then I close the wizard
    Given I have a "Wallet Sender" wallet with funds
    And I send 9 ADA from the "Wallet Sender" wallet to the "Wallet Receiver" wallet
    And I start the wallet delegation process
    Then I should see a "This wallet does not contain the minimum amount of 10 ADA which is required for delegation to be available. Please select a wallet with " message
    Given I close the wizard
    And I send 11 ADA from the "Wallet Sender" wallet to the "Wallet Receiver" wallet
    Then I sucessfully delegate my wallet

  @watch
  Scenario: "Delegation" wizard is showing the correct error message if the user submits wrong spending password
    Given I have the following wallets:
      | name            |
      | Wallet Receiver |
    And I have a "Wallet Sender" wallet with funds
    And I am on the Delegation "delegation-center" screen
    And I start the wallet delegation process
    And I choose the "Wallet Sender" wallet
    And I choose the first stake pool
    And I enter "INCORRECT" as the spending password
    Then I should see a "Incorrect wallet password." message

  # Scenario: "Delegation" wizard is showing correct stake pool tickers for the wallets in the wallet dropdown in case wallets are already delegated
  # Scenario: "Delegation" wizard is working correctly when the user is changing delegation preference (including a check for messages on the "Select stake pool" step)
  # Scenario: "Undelegate" wizard is working correctly (including showing the correct error message if the user submits a wrong spending password)
