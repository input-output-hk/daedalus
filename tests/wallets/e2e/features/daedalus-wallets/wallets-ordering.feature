@e2e
Feature: Wallets ordering

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test Wallet |

  Scenario: "Daedalus Balance" wallet is shown on the bottom of the list below "Daedalus Rewards" wallet in order of creation
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
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification
    And I restore "Rewards Wallet" wallet with funds
    Then "Daedalus Balance wallet" wallet should show on the bottom of the list below Rewards wallet

