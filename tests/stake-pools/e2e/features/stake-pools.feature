@e2e
Feature: Stake Pools Loading

  Background:
    Given I have completed the basic setup
    And I have a "Test Wallet" wallet with funds
    And I am on the Delegation Center screen

  Scenario: "Loading stake pools" message is shown during initial loading of stake pool data
    When I click on stake pools tab button
    And I am on the Staking pool screen
    Then I should see the following loading message:
      | message                                     |
      | staking.stakePools.loadingStakePoolsMessage |
    And I should't see loading message anymore
    And I should see stake pools listed

  Scenario: Stake pools load error handling works as expected
    Given I set stake pools fetch failed
    When I click on stake pools tab button
    And I am on the Staking pool screen
    And I should see the following loading message:
      | message                                     |
      | staking.stakePools.loadingStakePoolsMessage |
    And I should not see any stake pool

  Scenario: Stake pools are correctly ordered by rank
    When I click on stake pools tab button
    And I am on the Staking pool screen
    Then I should see the following loading message:
      | message                                     |
      | staking.stakePools.loadingStakePoolsMessage |
    And I see "300" stake pools
    And I should see "300" stake pools loaded by rank

  Scenario: Stake pools search works as expected
    When I click on stake pools tab button
    And I am on the Staking pool screen
    And I see "300" stake pools
    When I see the stake pools search input field
    And I enter "FF" in search input field
    Then I should see message "Stake pools. Search results: (9)"
    And I should see stake pool with slug "IJTI"

  Scenario: Stake pool tooltip is correctly displayed and shows correct data
    When I click on stake pools tab button
    And I am on the Staking pool screen
    And I see "300" stake pools
    And I click on stake pool with order number 2
    Then I should see stake pool tooltip with order number "2"
    And Stake pool "2" tooltip shows correct data

  Scenario: Delegating to stake pool from "Stake pools" screen works as expected
    When I click on stake pools tab button
    And I am on the Staking pool screen
    And I see "300" stake pools
    And I should see "300" stake pools loaded by rank
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
    And I see following label on the dialog: "You have selected [MKXR] stake pool to delegate to for Test Wallet wallet"

  Scenario: Stake pools user is already delegating to are correctly displayed
    When I click on stake pools tab button
    And I am on the Staking pool screen
    And I see "300" stake pools
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
    And I see following label on the dialog: "You have selected [MKXR] stake pool to delegate to for Test Wallet wallet"
    And I click "continue" button
    And I enter staking pool spending password "Secret1234" and click "confirm" button
    Then I should see label: "Staking pools you are delegating to"
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
    And I see following label for already delegated stake pools: "You are already delegating Test Wallet wallet to [ROOT] stake pool"
