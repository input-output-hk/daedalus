@unit
Feature: Mnemonics Form Validation

  Scenario: Incomplete Mnemonic Provided
    Given I require 3 mnemonic words
    When I provide 2 mnemonic words
    And I provide an mnemonics validation function
    And I validate the mnemonics with these parameters
    Then the result should be an incomplete mnemonic marker
    And the mnemonics validator should not have been called

  Scenario: Complete Mnemonic Provided
    Given I require 3 mnemonic words
    When I provide 3 mnemonic words
    And I provide an mnemonics validation function
    And I validate the mnemonics with these parameters
    Then the mnemonics validator should have been called with the provided words
    And the result should be the return value of the mnemonics validator
