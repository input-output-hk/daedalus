@e2e
Feature: Wallet Settings - Recovery Phrase Verification

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name    |
      | Wallet  |

  Scenario: Recovery phrase correctly verified
    Given the last recovery phrase veryfication was done 400 days ago
    And I am on the "Wallet" wallet "settings" screen
    Then I should see a "Notification" recovery phrase veryfication feature
    When I click the recovery phrase veryfication button
    And I click the checkbox and Continue button
    # And I click on recovery phrase mnemonics in correct order
    Then I freeze
    # And click the Verify button
