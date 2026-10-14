@unit @conformance
Feature: CIP extension conformance

  Scenario: Historical client evidence remains intact alongside current normative contracts
    Given the recorded CIP client observations
    Then recorded adapter fixtures retain their historical byte hashes
    And recorded standard revisions retain their historical provenance
    And unavailable live wallets are not reported as passing

  Scenario: Current public methods preserve normative wire contracts
    Then current public methods and wire behavior satisfy the normative contract
    And current enable and collateral semantics are enforced

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
    Given the current extension registry
    Then CIP-104 remains proposed and policy-gated
    And CIP-142 remains proposed and policy-gated
