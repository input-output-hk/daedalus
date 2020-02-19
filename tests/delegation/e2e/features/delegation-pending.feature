@e2e @skip
Feature: Wallet Pending Delegations

  Scenario Outline: Pending delegations
    Given I have completed the basic setup
    And I have the following "Rewards" wallets:
      | name              |
      | Unmodified Wallet |
    And I am on the Delegation "delegation-center" screen
    And I mark experimental feature as read
    Given the wallet has the following <DELEGATION_SCENARIO>
    Then I freeze
    # Then the wallet should correctly display the correct stake pool tickers
    # And the ADA logo should be <ACTIVE_POOL_VISIBILITY>
    # And the tickers should display the correct <TOOLTIPS>
    # And the wallet should display the correct <LINKS>

    Examples:
    | DELEGATION_SCENARIO                   | ACTIVE_POOL_VISIBILITY | TOOLTIPS                                  | LINKS                    |
    | undelegated                           | hidden                 | none                                      | Delegate                 |
    | undelegated > delegated               | hidden                 | none > from_epoch                         | Undelegate or Redelegate |
    | undelegated > delegated > undelegated | hidden                 | none > from_epoch > from_epoch            | Delegate                 |
    | undelegated > delegated > delegated   | hidden                 | none > from_epoch > from_epoch            | Undelegate or Redelegate |
    | delegated                             | visible                | earning_rewards                           | Undelegate or Redelegate |
    | delegated > undelegated               | visible                | earning_rewards > from_epoch              | Delegate                 |
    | delegated > undelegated > delegated   | visible                | earning_rewards > from_epoch              | Delegate                 |
    | delegated > delegated                 | visible                | earning_rewards > from_epoch              | Undelegate or Redelegate |
    | delegated > delegated > undelegated   | visible                | earning_rewards > from_epoch > from_epoch | Delegate                 |
    | delegated > delegated > delegated     | visible                | earning_rewards > from_epoch > from_epoch | Undelegate or Redelegate |
