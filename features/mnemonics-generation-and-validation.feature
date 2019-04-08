@unit @slow
Feature: Mnemonics generation and validation

  As a developer I want to be sure our mnemonic
  (12-word recovery phrase) is correctly generated/validated

  Scenario: All generated wallet recovery mnemonics are valid
    Given I generate 10000 wallet recovery mnemonics
    Then all generated wallet recovery mnemonics should be valid
