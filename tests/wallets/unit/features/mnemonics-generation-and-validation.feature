Feature: Mnemonics generation and validation

  As a developer I want to be sure our mnemonic
  (15-word recovery phrase) is correctly generated/validated

  @unit @slow
  Scenario: All generated wallet recovery mnemonics are valid
    Given I generate 10000 wallet recovery mnemonics
    Then all generated wallet recovery mnemonics should be valid

  @unbound @mnemonics
  Scenario: Unbound manual test run gives no invalid mnemeonics
    Given I generate and validate an unbound number of wallet recovery mnemonics
