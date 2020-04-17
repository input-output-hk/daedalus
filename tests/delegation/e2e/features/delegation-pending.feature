@e2e @shelley
Feature: Wallet Pending Delegations

  Scenario: Pending delegations
    Given I have completed the basic setup
    # The wallets don't need to be sequentially created,
    # as their data is changed by the patched api
    # so it doesn't matter their original names & data
    And I have the following wallets:
      | name           |
      | Test Wallet 1  |
      | Test Wallet 2  |
      | Test Wallet 3  |
      | Test Wallet 4  |
      | Test Wallet 5  |
      | Test Wallet 6  |
      | Test Wallet 7  |
      | Test Wallet 8  |
      | Test Wallet 9  |
      | Test Wallet 10 |
    And I am on the Delegation "delegation-center" screen
    And I mark experimental feature as read
    Given the wallets have the following pending delegations:
      | DELEGATION_SCENARIO                   |
      | undelegated                           |
      | undelegated > delegated               |
      | undelegated > delegated > undelegated |
      | undelegated > delegated > delegated   |
      | delegated                             |
      | delegated > undelegated               |
      | delegated > undelegated > delegated   |
      | delegated > delegated                 |
      | delegated > delegated > undelegated   |
      | delegated > delegated > delegated     |
    Then the wallets should correctly display the correct stake pool tickers
    And the ADA logo should be displayed as follows:
      | ADA_LOGO |
      | hidden   |
      | hidden   |
      | hidden   |
      | hidden   |
      | visible  |
      | visible  |
      | visible  |
      | visible  |
      | visible  |
      | visible  |
    And the tooltips should be displayed as follows:
      | TOOLTIPS                                  |
      | none                                      |
      | none > from_epoch                         |
      | none > from_epoch > from_epoch            |
      | none > from_epoch > from_epoch            |
      | earning_rewards                           |
      | earning_rewards > from_epoch              |
      | earning_rewards > from_epoch > from_epoch |
      | earning_rewards > from_epoch              |
      | earning_rewards > from_epoch > from_epoch |
      | earning_rewards > from_epoch > from_epoch |
    And the action links should be displayed as follows:
      | LINKS                    |
      | Delegate                 |
      | Undelegate or Redelegate |
      | Delegate                 |
      | Undelegate or Redelegate |
      | Undelegate or Redelegate |
      | Delegate                 |
      | Delegate                 |
      | Undelegate or Redelegate |
      | Delegate                 |
      | Undelegate or Redelegate |
