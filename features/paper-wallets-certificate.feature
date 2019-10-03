@e2e
Feature: Paper Wallets Certificate generation

  Background:
    Given I have completed the basic setup
    And I have a "Test Wallet" wallet with funds

  Scenario: Paper wallets certificate success generation
    Given The sidebar shows the "wallets" category
    And I click on the "paper-wallet-create-certificate" category in the sidebar
    And I see the "Certificate Generation Instructions" dialog
    And I click on the print button
    And I see the "Certificate Generation Complete" dialog
    And I check all "Print Dialog" checkboxes
    And I click on the continue button
    And I see the "Securing Additional mnemonics" dialog
    And I click on "I have written the remaining 9 words to the certificate." checkbox
    And I click on the continue button
    And I see the "Verify Certificate" dialog
    And I enter paper wallet recovery phrase
    And Verify certificate checkboxes are no longer disabled
    And I check all "Verify Certificate" checkboxes
    And I click on the continue button
    And I see the "Paper Wallet Certificate" dialog
    And Cardano explorer link and wallet address should be valid
    And I click on the finish button
    And I should not see the create paper wallet certificate dialog anymore
    When I click on the "Test Wallet" wallet in the sidebar
    And I am on the "Test Wallet" wallet "send" screen
    And I fill out the send form:
      | amount   |
      | 0.000010 |
    And the transaction fees are calculated
    And I click on the next button in the wallet send form
    And I see send money confirmation dialog
    And I submit the wallet send form
    Then I should be on the "Test Wallet" wallet "summary" screen
    And the latest transaction should show:
      | title                   | amountWithoutFees |
      | wallet.transaction.sent | -0.000010         |
    And I click on the add wallet button in the sidebar
    And I see the add wallet page
    And I click on the restore wallet button on the add wallet page
    And I see the restore wallet dialog
    And I click "Paper wallet certificate" tab
    And I see "Restore wallet with certificate" form
    And I enter wallet name "Restored CERTIFICATE wallet" in restore wallet dialog
    And I enter paper wallet recovery phrase
    And I toggle "Spending password" switch on the restore wallet with certificate dialog
    And I submit the restore wallet dialog
    Then I should not see the restore wallet dialog anymore
    And I should have newly created "Restored CERTIFICATE wallet" wallet loaded
    And I should be on the "Restored CERTIFICATE wallet" wallet "summary" screen
    And the balance of "Restored CERTIFICATE wallet" wallet should be:
      | balance  |
      | 0.000010 |
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    When I click the wallet receive button
    Then I should be on the "Restored CERTIFICATE wallet" wallet "receive" screen
    And I should see that address was used

  Scenario: Paper wallets certificate generation with wrong shielded recovery phrase
    Given The sidebar shows the "wallets" category
    And I click on the "paper-wallet-create-certificate" category in the sidebar
    And I see the "Certificate Generation Instructions" dialog
    And I click on the print button
    And I see the "Certificate Generation Complete" dialog
    And I check all "Print Dialog" checkboxes
    And I click on the continue button
    And I see the "Securing Additional mnemonics" dialog
    And I click on "I have written the remaining 9 words to the certificate." checkbox
    And I click on the continue button
    And I see the "Verify Certificate" dialog
    And I enter wrong paper wallet recovery phrase:
      | recoveryPhrase                                                                                                                                                  |
      | worry pluck anchor recycle predict grow inner inside face face subway meat away once february family rug make hub violin riot around coast play pluck grow face |
    And I should see the following field error message:
      | message                       |
      | global.errors.invalidMnemonic |
    And Verify certificate checkboxes should be disabled
    And Continue button should be disabled
