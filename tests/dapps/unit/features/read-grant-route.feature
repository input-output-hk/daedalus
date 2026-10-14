@unit
Feature: dApp read grant route authority

  Scenario: Active wallet route changes replace authority
    Given dApp route authority is configured for the current network
    When the trusted UI opens the dApp route for wallet "wallet-a"
    Then the active dApp route is bound to wallet "wallet-a"
    When the trusted UI opens the dApp route for wallet "wallet-b"
    Then the previous dApp route authority is stale
    And the active dApp route is bound to wallet "wallet-b"

  Scenario: A persisted read grant is reused after restart
    Given a read grant exists for wallet "wallet-a" and origin "https://dapp.example"
    When the grant repository restarts
    Then the read grant is reusable for wallet "wallet-a" and origin "https://dapp.example"
