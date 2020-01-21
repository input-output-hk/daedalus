@e2e
Feature: Stake Pools Loading

  Background:
    Given I have completed the basic setup
    Given I am on the Delegation Center screen
    And I click on stake pools tab button
    And I am on the Staking pool screen
    And I have a "Test Wallet" wallet with funds

  Scenario: "Loading stake pools" message is shown during initial loading of stake pool data
    Then I should see the following loading message:
      | message                               |
      | staking.stakePools.loadingStakePoolsMessage |

  Scenario: Stake pools load error handling works as expected
    And I should see the following loading message:
      | message                               |
      | staking.stakePools.loadingStakePoolsMessage |
    And Stake pools loading failed
    Then I should see loading stake pools error message:
      | message                               |
      | staking.stakePools.loadingStakePoolsMessage |
    And I should not see any stake pool

  Scenario: Stake pools are correctly ordered by rank
    Then I should see the following loading message:
      | message                               |
      | staking.stakePools.loadingStakePoolsMessage |
    And I see "3" stake pools
    And I should see "3" stake pools loaded by rank

  Scenario: Stake pools search works as expected
    And I see "3" stake pools
    When I see the stake pools search input field
    And I enter "ROOT" in search input field
    Then I should see message "Stake pools. Search results: (3)"
    And I should see number 3 stake pool with slug "ROOT"

  @watch
  Scenario: Stake pool tooltip is correctly displayed and shows correct data
    And I see "3" stake pools
    And I click on stake pool with order number 2
    Then I should see stake pool tooltip with order number "2"
    And Stake pool "2" tooltip shows correct data

  Scenario: Delegating to stake pool from "Stake pools" screen works as expected
    And I see "3" stake pools
    And I should see "3" stake pools loaded by rank
    And I click on stake pool with order number 2
    Then I should see stake pool tooltip with order number "2"
    And Stake pool "2" tooltip shows correct data
    And I click on "Delegate to this pool"
    Then I should see "Delegate Wallet" dialog
    And I click "continue" button
    Then I should see step 1 of 3 screen
    And I open the wallet dropdown
    And I choose "Test Wallet"
    And I click "continue" button
    Then I should see step 2 of 3 screen
    And I see following label on the dialog: "You have selected [ROOT] stake pool to delegate to for Test Wallet wallet.,[ROOT],Test Wallet"

  Scenario: Stake pools user is already delegating to are correctly displayed
