@e2e @skip
# @API TODO - we don't have request for `getWalletIdAndBalance`
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
    And I enter the recovery phrase mnemonics correctly
    Then I should see the confirmation dialog
    When I click the checkbox and Continue button
    Then I should not see any dialog
    And I should see a "Ok" recovery phrase veryfication feature

  Scenario: Recovery phrase incorrectly verified
    Given the last recovery phrase veryfication was done 200 days ago
    And I am on the "Wallet" wallet "settings" screen
    Then I should see a "Warning" recovery phrase veryfication feature
    When I click the recovery phrase veryfication button
    And I click the checkbox and Continue button
    And I enter the recovery phrase mnemonics incorrectly
    Then I should see the error dialog
    When I click the close button
    Then I should not see any dialog
