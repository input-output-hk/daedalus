@unit @conformance
Feature: CIP extension conformance

  Scenario: Frozen contracts match current authoritative revisions
    Given the recorded CIP client observations
    Then every public provider method matches the frozen contract
    And current standard revisions match the frozen provenance
    And unavailable live wallets are not reported as passing

  Scenario: Cardano JS SDK compatible CIP-95 invocation and encoding
    Given the source-verified Cardano JS SDK CIP-95 adapter
    When the adapter invokes the CIP-95 namespace
    Then it uses the frozen CIP-95 public method names
    And raw and matching type-6 DRep inputs produce the frozen normalized COSE

  Scenario: CIP-103 clients receive the normative mixed rejection
    Given the source-verified CIP-103 client adapter
    When the adapter receives a mixed submission failure
    Then it catches the aligned mixed array directly

  Scenario: Proposed extensions retain their release gates
    Given the frozen extension registry
    Then CIP-104 remains terminal-disabled and omitted
    And CIP-142 remains proposed and policy-gated
