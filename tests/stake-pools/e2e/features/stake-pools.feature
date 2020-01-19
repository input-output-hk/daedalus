@e2e
Feature: Stake Pools Loading

  Background:
    Given I have completed the basic setup
    Given I am on the Delegation Centre staking page
    Given I have a "Test Wallet" wallet with funds
    And I have the following wallets:
      | name          |
      | Test Wallet |

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
    And I should see "3" stake pools loaded by rank

  Scenario: Stake pools search works as expected
    When I see the stake pools search input field
    And I enter "ROOT" in search input field
    Then I should see message "Stake pools. Search results: (3)"
    And I should see "3" stake pool with slug "ROOT"

  Scenario: Stake pool tooltip is correctly displayed and shows correct data
    When I see "3" stake pools loaded by rank
    And I click on stake pool on second place
    Then I should see second stake pool tooltip
    And Stake pool "2" tooltip shows correct data

  @watch
  Scenario: Delegating to stake pool from "Stake pools" screen works as expected
    When I see "3" stake pools loaded by rank
    And I click on stake pool on second place
    Then I should see second stake pool tooltip
    And Stake pool "2" tooltip shows correct data
    And I click on "Delegate to this pool"
    Then I should see "Delegate Wallet" dialog
    #And I click "continue" button
    #Then I should see step 1 of 3 screen
    #And I choose the wallet which i want to delegate
    #And I click "continue" button
    #Then I should see step 2 of 3 screen
    #And I pick one of stake pools
    #And I click on "continue" button
    #Then I should see step 3 of 3 screen
    #And I type spending password
    #And I click "continue" button
    #Then I should see "Wallet Delegated" confirmation screen
    #And I click "close" button
    #Then I should see "Delegated to..." message next to wallet which has been delegated

  Scenario: Delegating to stake pool from "Stake pools" screen works as expected with different selection
    When I see "3" stake pools loaded by rank
    And I click on stake pool on second place
    Then I should see second stake pool tooltip
    And Stake pool "2" tooltip shows correct data
    And I click on "Delegate to this pool"
    Then I should see "Delegate Wallet" dialog
    #And I click "continue" button
    #Then I should see step 1 of 3 screen
    #And I choose the wallet which i want to delegate
    #And I click "continue" button
    #Then I should see step 2 of 3 screen
    #And I pick one of stake pools
    #And I click on "continue" button
    #Then I should see step 3 of 3 screen
    #And I type spending password
    #And I click "continue" button
    #Then I should see "Wallet Delegated" confirmation screen
    #And I click "close" button
    #Then I should see "Delegated to..." message next to wallet which has been delegated
