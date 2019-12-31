@e2e
@watch
Feature: Add Wallet via Sidebar

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test Wallet |

  Scenario: Successfully restoring Daedalus Balance wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Daedalus wallet"
    Then I should see section "What kind of Daedalus wallet would you like to restore?"
    Then I click on option "12 words"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                  |
      | prison census discover give sound behave hundred cave someone orchard just wild |
    And I click Check recovery phrase button
    And I enter wallet name "Daedalus Balance wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Daedalus Balance wallet" wallet loaded
    And "Daedalus Balance wallet" wallet should have "legacy_6eb9a6862e5656b4a52fa6fae8eb3a3e8f7c2bd6" as id
    And I should be on the "Daedalus Balance wallet" wallet "summary" screen
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished

  Scenario: Successfully restoring Daedalus Rewards wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Daedalus wallet"
    Then I should see section "What kind of Daedalus wallet would you like to restore?"
    Then I click on option "15 words"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                             |
      | combine mouse cool skirt truck outer result speed fringe sugar there usage lucky wild tail |
    And I click Check recovery phrase button
    And I enter wallet name "Daedalus Rewards wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Daedalus Rewards wallet" wallet loaded
    And "Daedalus Rewards wallet" wallet should have "c2ebd8b727cc760fe2f0fb3d06a8630ccc8c70f5" as id

  Scenario: Successfully restoring Daedalus paper wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Daedalus wallet"
    Then I should see section "What kind of Daedalus wallet would you like to restore?"
    Then I click on option "27 words"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | season nice police near blame dress deal congress unusual more giggle pull general list crash gravity fashion notable voice resemble auto smart flat party thought unique amused |
    And I click Check recovery phrase button
    And I enter wallet name "Daedalus paper wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Daedalus paper wallet" wallet loaded
    And "Daedalus paper wallet" wallet should have "legacy_699c20fef5469d2cabadf5a778932d06ca3364e2" as id

  Scenario: Successfully restoring Yoroi Balance paper wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Yoroi wallet"
    Then I should see section "What kind of Yoroi wallet would you like to restore?"
    Then I click on option "(Balance wallet)"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | defense brush fiscal cactus rotate trouble mean quantum shrug slight dignity corn immense first citizen |
    And I click Check recovery phrase button
    And I enter wallet name "Yoroi Balance wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Yoroi Balance wallet" wallet loaded
    And "Yoroi Balance wallet" wallet should have "legacy_aab5517861cca76a53d83e24c84542ecac6c0a3d" as id

  Scenario: Successfully restoring Yoroi Rewards paper wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Yoroi wallet"
    Then I should see section "What kind of Yoroi wallet would you like to restore?"
    Then I click on option "(Rewards wallet)"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | defense brush fiscal cactus rotate trouble mean quantum shrug slight dignity corn immense first citizen |
    And I click Check recovery phrase button
    And I enter wallet name "Yoroi Rewards wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Yoroi Rewards wallet" wallet loaded
    And "Yoroi Rewards wallet" wallet should have "aab5517861cca76a53d83e24c84542ecac6c0a3d" as id

  Scenario: Successfully restoring 12-word Ledger wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Hardware wallet"
    Then I should see section "What kind of hardware wallet would you like to restore?"
    Then I click on option "Ledger Nano"
    Then I confirm "I understand and accept responsibility for the security concerns"
    Then I confirm "I understand that I should delete the Balance wallet"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | struggle section scissors siren garbage yellow maximum finger duty require mule earn |
    And I click Check recovery phrase button
    And I enter wallet name "12-word Ledger wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "12-word Ledger wallet" wallet loaded
    And "12-word Ledger wallet" wallet should have "legacy_64c76f5644be19e5ba4cbe717967e2fd057079b3" as id

  Scenario: Successfully restoring 18-word Ledger wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Hardware wallet"
    Then I should see section "What kind of hardware wallet would you like to restore?"
    Then I click on option "Ledger Nano"
    Then I confirm "I understand and accept responsibility for the security concerns"
    Then I confirm "I understand that I should delete the Balance wallet"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | vague wrist poet crazy danger dinner grace home naive unfold april exile relief rifle ranch tone betray wrong |
    And I click Check recovery phrase button
    And I enter wallet name "18-word Ledger wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "18-word Ledger wallet" wallet loaded
    And "18-word Ledger wallet" wallet should have "legacy_4a77dabe7d12477beb9c2952da1d9d93fe7eb180" as id

  Scenario: Successfully restoring 24-word Ledger wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Hardware wallet"
    Then I should see section "What kind of hardware wallet would you like to restore?"
    Then I click on option "Ledger Nano"
    Then I confirm "I understand and accept responsibility for the security concerns"
    Then I confirm "I understand that I should delete the Balance wallet"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | recall grace sport punch exhibit mad harbor stand obey short width stem awkward used stairs wool ugly trap season stove worth toward congress jaguar |
    And I click Check recovery phrase button
    And I enter wallet name "24-word Ledger wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "24-word Ledger wallet" wallet loaded
    And "24-word Ledger wallet" wallet should have "legacy_39a4b214546f40a1117b77fbc35bd8c6b5425b2a" as id

  Scenario: Successfully restoring 12-word Trezor wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Hardware wallet"
    Then I should see section "What kind of hardware wallet would you like to restore?"
    Then I click on option "Trezor (Balance wallet)"
    Then I confirm "I understand and accept responsibility for the security concerns"
    Then I confirm "I understand that I should delete the Balance wallet"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | walk license firm dwarf hundred pride ensure midnight unit keen warfare east |
    And I click Check recovery phrase button
    And I enter wallet name "12-word Trezor wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "12-word Trezor wallet" wallet loaded
    And "12-word Trezor wallet" wallet should have "legacy_a8f17df699d35d4541dd385b08a376dfe25b53e5" as id

  Scenario: Successfully restoring 18-word Trezor wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Hardware wallet"
    Then I should see section "What kind of hardware wallet would you like to restore?"
    Then I click on option "Trezor (Balance wallet)"
    Then I confirm "I understand and accept responsibility for the security concerns"
    Then I confirm "I understand that I should delete the Balance wallet"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | hen idea mimic frog second magnet egg indicate jar girl broccoli heart verify person present toe vibrant unable |
    And I click Check recovery phrase button
    And I enter wallet name "18-word Trezor wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "18-word Trezor wallet" wallet loaded
    And "18-word Trezor wallet" wallet should have "legacy_f7528d9ff728aae94bae9679b3ad330d5bc2d63a" as id

  Scenario: Successfully restoring 24-word Trezor wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Hardware wallet"
    Then I should see section "What kind of hardware wallet would you like to restore?"
    Then I click on option "Trezor (Balance wallet)"
    Then I confirm "I understand and accept responsibility for the security concerns"
    Then I confirm "I understand that I should delete the Balance wallet"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | slot young shoot surround equal trouble rice update rare dinosaur drastic kitten mom actress salon abuse happy satisfy |
    And I click Check recovery phrase button
    And I enter wallet name "24-word Trezor wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "24-word Trezor wallet" wallet loaded
    And "24-word Trezor wallet" wallet should have "legacy_75ca580977d98216b0d2c96992250d3fa812f842" as id
