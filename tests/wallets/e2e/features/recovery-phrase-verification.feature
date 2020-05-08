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
    And I enter the "<RECOVERY_PHRASE>" recovery phrase mnemonics
    And I click the verify button
    Then I should see the confirmation dialog
    When I click the checkbox and Continue button
    Then I should not see any dialog
    And I should see a "Ok" recovery phrase veryfication feature
    Then I freeze

    Examples:
      | name                | kind     | subkind       | recovery_phrase                                                                                                                                                                  |
      | Daedalus-Balance-12 | Daedalus | Balance12Word | prison census discover give sound behave hundred cave someone orchard just wild                                                                                                  |
      | Daedalus-Rewards-15 | Daedalus | Reward15Word  | combine mouse cool skirt truck outer result speed fringe sugar there usage lucky wild tail                                                                                       |
      | Daedalus-Paper-27   | Daedalus | Balance27Word | season nice police near blame dress deal congress unusual more giggle pull general list crash gravity fashion notable voice resemble auto smart flat party thought unique amused |
      | Yoroi-Balance-15    | Yoroi    | Balance15Word | defense brush fiscal cactus rotate trouble mean quantum shrug slight dignity corn immense first citizen                                                                          |
      | Yoroi-Rewards-15    | Yoroi    | Reward15Word  | defense brush fiscal cactus rotate trouble mean quantum shrug slight dignity corn immense first citizen                                                                          |
      | Hardware-Ledger-12  | Hardware | Ledger        | struggle section scissors siren garbage yellow maximum finger duty require mule earn                                                                                             |
      | Hardware-Ledger-18  | Hardware | Ledger        | vague wrist poet crazy danger dinner grace home naive unfold april exile relief rifle ranch tone betray wrong                                                                    |
      | Hardware-Ledger-24  | Hardware | Ledger        | recall grace sport punch exhibit mad harbor stand obey short width stem awkward used stairs wool ugly trap season stove worth toward congress jaguar                             |
      | Hardware-Trezor-12  | Hardware | Trezor        | walk license firm dwarf hundred pride ensure midnight unit keen warfare east                                                                                                     |
      | Hardware-Trezor-18  | Hardware | Trezor        | hen idea mimic frog second magnet egg indicate jar girl broccoli heart verify person present toe vibrant unable                                                                  |
      | Hardware-Trezor-24  | Hardware | Trezor        | slot young shoot surround equal trouble rice update rare dinosaur drastic kitten mom actress salon abuse happy satisfy                                                           |

