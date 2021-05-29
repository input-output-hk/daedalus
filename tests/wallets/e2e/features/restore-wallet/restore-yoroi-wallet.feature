@e2e
Feature: Restore Yoroi wallet

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test Wallet |

  Scenario: Successfully restoring "Yoroi Byron" byron wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Yoroi wallet"
    Then I should see section "What kind of Yoroi wallet would you like to restore?"
    Then I click on option "(Byron wallet)"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | defense brush fiscal cactus rotate trouble mean quantum shrug slight dignity corn immense first citizen |
    And I click Check recovery phrase button
    And I enter wallet name "Yoroi Byron wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Yoroi Byron wallet" wallet loaded
    And "Yoroi Byron wallet" wallet should have "legacy_aab5517861cca76a53d83e24c84542ecac6c0a3d" as id
    And "Byron" wallet badge should be visible in the wallet sidebar
    And "Byron" wallet "Move testnet ada" action should be visible in the top bar notification

  Scenario: Successfully restoring "Yoroi Shelley" shelley wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Yoroi wallet"
    Then I should see section "What kind of Yoroi wallet would you like to restore?"
    Then I click on option "(Shelley wallet)"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                                                                                                   |
      | defense brush fiscal cactus rotate trouble mean quantum shrug slight dignity corn immense first citizen |
    And I click Check recovery phrase button
    And I enter wallet name "Yoroi Shelley wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Yoroi Shelley wallet" wallet loaded
    And "Yoroi Shelley wallet" wallet should have "aab5517861cca76a53d83e24c84542ecac6c0a3d" as id

  Scenario: Successfully restoring "Yoroi Byron" wallet
    Given The sidebar shows the "wallets" category
    When I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    Then I click on option "Yoroi wallet"
    And I click continue
    And I enter recovery phrase in restore wallet dialog:
      | recoveryPhrase                                                                                      |
      | frozen neck rural balcony rural into tired vibrant that trigger shadow avocado resemble cliff novel |
    And I click Check recovery phrase button
    And I enter wallet name "Yoroi Byron wallet" in restore wallet dialog
    And I enter wallet password in restore wallet dialog:
      | password  | repeatedPassword |
      | Secret1234 | Secret1234      |
    And I click continue
    Then I should see a screen titled "Wallet Restored"
    And I click close
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Yoroi Byron wallet" wallet loaded
    And "Yoroi Byron wallet" wallet should have "legacy_ca8f1986634654c7937c6f931872a1712998b17b" as id
