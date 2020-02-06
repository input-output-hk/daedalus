@e2e
Feature: Restore Hardware wallet

  Background:
    Given I have completed the basic setup
    And I have the following "Rewards" wallets:
      | name        |
      | Test Wallet |
  
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
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification

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
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification

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
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification

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
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification

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
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification

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
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification
    