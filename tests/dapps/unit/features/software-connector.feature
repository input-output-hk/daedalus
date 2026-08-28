@unit
Feature: Software dApp connector journeys

  Scenario: Connect, read, sign, and submit with separate approval
    Given an eligible software wallet and dApp request
    When the user connects the dApp
    Then the dApp can read the selected wallet network
    When the user approves exact data and transaction signing
    Then the connector returns locally verified CIP-8 data and transaction witnesses
    When the user separately approves exact transaction submission
    Then the connector returns the locally derived transaction id

  Scenario: Negotiate and use CIP-95 governance signing
    Given an eligible software wallet and dApp request
    When the user connects the dApp with CIP-95
    And separately approves governance key disclosure
    Then raw and type-6 DRep signing inputs produce the same verified identity

  Scenario: Prepare and adopt preferred collateral after confirmation
    Given an eligible software wallet without sufficient collateral
    When the user explicitly starts collateral preparation
    Then the normal confirmed Send flow is required
    When the submitted preparation output is confirmed
    Then the confirmed output becomes the preferred collateral
