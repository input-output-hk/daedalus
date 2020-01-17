@e2e
Feature: Stake Pools Loading

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name          |
      | Test Wallet |

  @watch
  Scenario: "Loading stake pools" message is shown during initial loading of stake pool data
    Given The sidebar shows "Delegation Center" staking page icon
    When I click on the "Delegation Center" staking page button
    And I see the "Delegation Center" staking page
    And I click on the "Stake Pools" tab
    And I see the "Stake Pools" page
    Then I should see the "Loading stake pools" message

  Scenario: Stake pools load error handling works as expected
    Given The sidebar shows "Delegation Center" staking page icon
    When I click on the "Delegation Center" staking page button
    And I see the "Delegation Center" staking page
    And I click on the "Stake Pools" tab
    And I see the "Stake Pools" page
    And I see the "Loading stake pools" message
    Then I should see loading stake pools error message
    And I should not see any stake pools

  Scenario: Stake pools are correctly ordered by rank
    Given The sidebar shows "Delegation Center" staking page icon
    When I click on the "Delegation Center" staking page button
    And I see the "Delegation Center" staking page
    And I click on the "Stake Pools" tab
    And I see the "Stake Pools" page
    #And I see the "Loading stake pools" message
    And I should see "3" stake pools loaded by rank

  Scenario: Stake pools search works as expected
    Given The sidebar shows "Delegation Center" staking page icon
    When I click on the "Delegation Center" staking page button
    And I see the "Delegation Center" staking page
    And I click on the "Stake Pools" tab
    And I see the "Stake Pools" page
    And I see the stake pools search input field
    And I execute search in the stake pool search input field //@todo - search for xyz
    And There are results of the search //@todo - search for xyz
    Then I should see filtered stake pools as results //@todo - search for xyz

  Scenario: Stake pools user is already delegating to are correctly displayed
    Given The sidebar shows "Delegation Center" staking page icon
    When I click on the "Delegation Center" staking page button
    And I see the "Delegation Center" staking page
    And I see list of wallets
    And If the user is already delegating to any of stake pools
    Then I should see the "You are already delegating..." message next to the wallet which is already delegating

  Scenario: Stake pool tooltip is correctly displayed and shows correct data
    Given The sidebar shows "Delegation Center" staking page icon
    When I click on the "Delegation Center" staking page button
    And I see the "Delegation Center" staking page
    And I click on the "Stake Pools" tab
    And I see the "Stake Pools" page
    And I see any stake pool
    And I click on any stake pool
    Then I should see stake pool tooltip
    And Stake pool tooltip shows correct data

  Scenario: Delegating to stake pool from "Stake pools" screen works as expected
    Given The sidebar shows "Delegation Center" staking page icon
    When I click on the "Delegation Center" staking page button
    And I see the "Delegation Center" staking page
    And I see Not-delegated label
    And I click on "Delegate your stake"
    Then I should see "Delegate Wallet" modal
    And I click "continue" button
    Then I should see step 1 of 3 screen
    And I choose the wallet which i want to delegate
    And I click "continue" button
    Then I should see step 2 of 3 screen
    And I pick one of stake pools
    And I click on "continue" button
    Then I should see step 3 of 3 screen
    And I type spending password
    And I click "continue" button
    Then I should see "Wallet Delegated" confirmation screen
    And I click "close" button
    Then I should see "Delegated to..." message next to wallet which has been delegated
