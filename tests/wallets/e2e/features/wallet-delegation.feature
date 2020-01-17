@e2e
Feature: Wallet Delegation

  Background:
    Given I have completed the basic setup

  # TODO:
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

  Scenario: Delegation screen current and next epoch countdown are correctly displayed and have correct data
    Given I have the following wallets:
      | name   |
      | Wallet |
    And I am on the Delegation "delegation-center" screen
    Then the current and next epoch countdown are correctly displayed
    # TODO:
    # And the current and next epoch countdown have correct data

    @watch
  Scenario: "Delegate", "Change delegation" "Undelegate" options are correctly displayed on the "Delegation center" screen
    Given I have the following wallets:
      | name   |
      | Wallet |
    And I am on the Delegation "delegation-center" screen
    Then I should see the "delegate" option

  # Scenario: Delegated wallets information is correctly displayed on the "Delegation center" screen (Stake pool ticker is correctly shown and the "Change delegation" / "Undelegate" options are available

  # Scenario: "Unknown" stake pool is shown for the wallets being delegated to stake pools for which we don't have metadata
  # Scenario: "Delegation" wizard displays the "Delegation is unavailable" message if all of the Rewards wallet user has are empty
  # Scenario: "Delegation" wizard is not allowing delegation if the user selects a wallet which has less than 10 ADA
  # Scenario: "Delegation" wizard is working correctly if the user selects wallet with enough funds (including a check for fees estimation)
  # Scenario: "Delegation" wizard is showing the correct error message if the user submits wrong spending password
  # Scenario: "Delegation" wizard is showing correct stake pool tickers for the wallets in the wallet dropdown in case wallets are already delegated
  # Scenario: "Delegation" wizard is working correctly when the user is changing delegation preference (including a check for messages on the "Select stake pool" step)
  # Scenario: "Undelegate" wizard is working correctly (including showing the correct error message if the user submits a wrong spending password)
