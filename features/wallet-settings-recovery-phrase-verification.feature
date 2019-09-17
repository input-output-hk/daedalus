@e2e @watch
Feature: Wallet Settings - Recovery Phrase Verification

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name   | password  |
      | first  |           |
      | second | Secret123 |

  Scenario Outline: Wallet recovery phrase was never checked
    Given I freeze
    # Given I have neved checked the wallet recovery phrase
    # Given I have the following wallets:
    #   | name   |
    #   | Wallet |
    # And I am on the "<START>" wallet "summary" screen
    # And The sidebar shows the "wallets" category
    # And the sidebar submenu is visible
    # When I click on the "<TARGET>" wallet in the sidebar
    # Then I should be on the "<TARGET>" wallet "summary" screen

    Examples:
    | START  | TARGET |
    | first  | second |
    | second | third  |
