Feature: Scrambling and Unscrambling mnemonics

  As a developer I want to be sure mnemonics are correctly scrambled and unscrambled

  @unit
  Scenario: Scramble and Unscramble mnemonics
    Given I generate 12 word mnemonic
    And I generate additional mnemonic words
    And I generate spending password from 9-word mnemonic
    And I generate 18-word scrambled mnemonic
    And I generate 27-word paper wallet certificate recovery phrase
    And I unscramble mnemonics
    Then Unscrambled mnemonic should be same as generated 12-word mnemonic
