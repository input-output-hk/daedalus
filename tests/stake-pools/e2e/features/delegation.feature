@e2e @skip
Feature: Pending Delegation

  Scenario Outline: Pending delegations
    Given I have completed the basic setup
    And I have the following "Rewards" wallets:
      | name          |
      | Unmodified Wallet |
    And I am on the Delegation "delegation-center" screen
    And I mark experimental feature as read
    Given the wallet has <CURRENT_EPOCH>, <NEXT_EPOCH> and <LAST_EPOCH> delegation data
    Then the Pending Delegation information should be <SHOW_PENDING_MENU>
    And the wallet should be displayed as <DISPLAY_AS_DELEGATED>

    Examples:
    | CURRENT_EPOCH  | NEXT_EPOCH     | LAST_EPOCH     | SHOW_PENDING_MENU | DISPLAY_AS_DELEGATED |
    | not_delegating | none           | none           | hidden            | not_delegating       |
    | delegating     | none           | none           | hidden            | delegating           |
    | delegating     | delegating     | none           | visible           | delegating           |
    | delegating     | not_delegating | none           | visible           | not_delegating       |
    | not_delegating | delegating     | not_delegating | visible           | not_delegating       |
    | not_delegating | delegating     | delegating     | visible           | delegating           |
    | delegating     | not_delegating | delegating     | visible           | delegating           |
    | delegating     | delegating     | not_delegating | visible           | not_delegating       |
