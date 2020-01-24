@e2e @watch
Feature: Stake Pools Loading

  Background:
    Given I have completed the basic setup
    And I have a "Test Wallet" wallet with funds
    And I am on the Delegation Center screen

  Scenario: Stake pools are correctly loaded and ordered by rank
    When I click on stake pools tab button
    And I am on the Staking pool screen
    Then I should see stake pools listed
    And I should see stake pools ordered by rank

  Scenario: Stake pools load error handling works as expected
    Given I set stake pools fetch failed
    When I click on stake pools tab button
    And I am on the Staking pool screen
    Then I should see the following loading message:
      | message                                     |
      | staking.stakePools.loadingStakePoolsMessage |
    And I should not see any stake pool
  
  Scenario: Stake pools search works as expected
    When I click on stake pools tab button
    And I am on the Staking pool screen
    Then I should see stake pools listed
    When I see the stake pools search input field
    And I enter "ROOT" in search input field
    Then I should see search label with text: "Stake pools. Search results: (3)"
    And I should see stake pool with slug "ROOT"

  Scenario: Stake pool tooltip is correctly displayed and shows correct data
    When I click on stake pools tab button
    And I am on the Staking pool screen
    Then I should see stake pools listed
    When I click on stake pool with order number "2"
    Then I should see stake pool tooltip with order number "2"
    And Stake pool with rank "2" tooltip shows correct data

  Scenario: Delegating to stake pool from "Stake pools" screen works as expected
    When I click on stake pools tab button
    And I am on the Staking pool screen
    Then I should see stake pools listed
    When I click on stake pool with order number "2"
    Then I should see stake pool tooltip with order number "2" 
    And Stake pool with rank "2" tooltip shows correct data
    And I click on "Delegate to this pool"
    Then I should see "Delegate Wallet" dialog
    And I click "continue" button
    Then I should see step 1 of 3 screen
    And I open the wallet dropdown
    And I choose "Test Wallet" wallet
    And I click "continue" button
    Then I should see step 2 of 3 screen
    And I see following label on the dialog: "You have selected [ROOT] stake pool to delegate to for Test Wallet wallet"

  Scenario: Stake pools user is already delegating to are correctly displayed
    When I click on stake pools tab button
    And I am on the Staking pool screen
    Then I should see stake pools listed
    And I have a wallet "Test Wallet" delegated to stake pool with rank "2"
    Then I should see label: "Staking pools you are delegating to"
    And I click on delegated stake pool
    Then I should see stake pool tooltip with order number "2"
    And Stake pool with rank "2" tooltip shows correct data
    And I click on "Delegate to this pool"
    Then I should see "Delegate Wallet" dialog
    And I click "continue" button
    Then I should see step 1 of 3 screen
    And I open the wallet dropdown
    And I choose "Test Wallet" wallet
    And I click "continue" button
    Then I should see step 2 of 3 screen
    And I see following label on the dialog: "You are already delegating Test Wallet wallet to [ROOT] stake pool"
    And Continue button should be disabled
