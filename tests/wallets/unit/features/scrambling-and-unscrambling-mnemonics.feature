Feature: Scrambling and Unscrambling mnemonics

  As a developer I want to be sure mnemonics are correctly scrambled and unscrambled

  @unit
  Scenario: Scramble mnemonics
    Given I generate 1 wallet recovery mnemonics
    And all generated wallet recovery mnemonics should be valid
    And I generate additional mnemonic words
    And I generate spending password from 9-word mnemonic
    And I scramble mnemonics
    Then I should have 21 words mnemonic

  @unit
  Scenario: Unscramble mnemonics
    Given I have wallet certificate recovery phrase
    And I unscramble mnemonics
    Then I should have 12 words mnemonic
