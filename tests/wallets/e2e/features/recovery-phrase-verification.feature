@e2e
Feature: Wallet Settings - Recovery Phrase Verification

  Background:
    Given I have completed the basic setup

  Scenario: Recovery phrase correctly verified
    Given I have the following wallets:
      | name   |
      | Wallet |
    And the last recovery phrase veryfication was done 400 days ago
    And I am on the "Wallet" wallet "settings" screen
    Then I should see a "Notification" recovery phrase veryfication feature
    When I click the recovery phrase veryfication button
    And I click the checkbox and Continue button
    And I enter the recovery phrase mnemonics correctly
    And I click the verify button
    Then I should see the confirmation dialog
    When I click the checkbox and Continue button
    Then I should not see any dialog
    And I should see a "Ok" recovery phrase veryfication feature

  Scenario: Recovery phrase incorrectly verified
    Given I have the following wallets:
      | name   |
      | Wallet |
    And the last recovery phrase veryfication was done 200 days ago
    And I am on the "Wallet" wallet "settings" screen
    Then I should see a "Warning" recovery phrase veryfication feature
    When I click the recovery phrase veryfication button
    And I click the checkbox and Continue button
    And I enter the recovery phrase mnemonics incorrectly
    And I click the verify button
    Then I should see the error dialog
    When I click the close button
    Then I should not see any dialog

  Scenario Outline: Recovery phrase verification for all wallet kinds
    Given I have restored the "<name>" wallet of "<kind>" kind, "<subkind>" subkind and "<recovery_phrase>" recovery phrase
    And the last recovery phrase veryfication was done 200 days ago
    And I am on the "<name>" wallet "settings" screen
    Then I should see a "Warning" recovery phrase veryfication feature
    When I click the recovery phrase veryfication button
    And I click the checkbox and Continue button
    And I enter the "<recovery_phrase>" recovery phrase mnemonics
    And I click the verify button
    Then I should see the confirmation dialog
    When I click the checkbox and Continue button
    Then I should not see any dialog
    And I should see a "Ok" recovery phrase veryfication feature

    Examples:
      | name                | kind     | subkind       | recovery_phrase                                                                                                                                                                  |
      | Daedalus-Rewards-15 | Daedalus | Reward15Word  | combine mouse cool skirt truck outer result speed fringe sugar there usage lucky wild tail                                                                                       |
      | Yoroi-Rewards-15    | Yoroi    | Reward15Word  | defense brush fiscal cactus rotate trouble mean quantum shrug slight dignity corn immense first citizen                                                                          |
