@e2e
Feature: TEST
  Background:
    Given I have completed the basic setup

  @watch
  Scenario: Testing
    And I have the following wallets:
      | name   |
      | SHELLEY MULTIPLE  |
    And I have a "SHELLEY SINGLE" wallet
    And I have the following balance wallets:
      | name   |
      | BYRON MULTIPLE  |
      | BYRON MULTIPLE 2  |
      | BYRON MULTIPLE 3  |
    And I have a "BYRON SINGLE" balance wallet
  Then I freeze
