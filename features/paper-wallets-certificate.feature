Feature: Paper Wallets Certificate generation

  Background:
    Given I have completed the basic setup
    And I have a wallet with funds

  Scenario: Paper wallets certificate success generation
    Given The sidebar shows the "wallets" category
    And I click on the "paper-wallet-create-certificate" category in the sidebar
    And I see the "Certificate Generation Instructions" dialog
    And I click on the continue button
    And I see the "Password Choice" dialog
    And I enter shielded password:
      | password  | repeatedPassword |
      | Secret123 | Secret123        |
    And I click on "I understand the importance of the password and I will keep it secure." checkbox
    And Print button is no longer disabled
    And I click on the print button
    And I see the "Certificate Generation Complete" dialog
    And I click on "Yes, paper wallet certificate successfully printed and everything is readable and scannable." checkbox
    And Continue button is no longer disabled
    And I click on the continue button
    And I see the "Certificate Password" dialog
    And Shown password should be equal to entered password
    And I click on "I understand that I can not use my certificate without the password and I have stored it safely." checkbox
    And Continue button is no longer disabled
    And I click on the continue button
    And I see the "Verify Certificate" dialog
    And I enter shielded recovery phrase
    And I enter password "Secret123"
    And Checkboxes are no longer disabled
    And I click on "I understand that the created wallet will not be stored in Daedalus after this step." checkbox
    And I click on "I understand that my wallet can only be recovered using my paper wallet certificate and the password I have chosen." checkbox
    And Continue button is no longer disabled
    And I click on the continue button
    And I see the "Paper Wallet Certificate" dialog
    And Cardano explorer link should be valid
    And I click on the finish button
    And I should not see the create paper wallet certificate dialog anymore
    And I send money to paper wallet certificate dialog
    And The sidebar shows the "wallets" category
    And I click on the add wallet button in the sidebar
    And I see the add wallet dialog
    And I click on the restore wallet button in add wallet dialog
    And I see the restore wallet dialog
    And I click "Paper wallet certificate" tab
    And I see "Restore wallet with certificate" form
    And I enter wallet name "Restored CERTIFICATE wallet" in restore wallet dialog
    And I enter shielded recovery phrase
    And I enter paper wallet certificate password
    And I submit the restore wallet dialog
    Then I should see the restore status notification while restore is running
    And I should not see the restore wallet dialog anymore
    And I should not see the restore status notification once restore is finished
    And I should have newly created "Restored CERTIFICATE wallet" wallet loaded
    When I click on the "Restored CERTIFICATE wallet" wallet in the sidebar
    Then I should be on the "Restored CERTIFICATE wallet" wallet "summary" screen
    And the balance of "Restored CERTIFICATE wallet" wallet should be:
      | balance  |
      | 0.000020 |
    And I click the wallet receive button
    Then I should be on the "Restored CERTIFICATE wallet" wallet "receive" screen
    And I should see that address was used

  Scenario: Paper wallets certificate generation with wrong shielded recovery phrase
    Given The sidebar shows the "wallets" category
    And I click on the "paper-wallet-create-certificate" category in the sidebar
    And I see the "Certificate Generation Instructions" dialog
    And I click on the continue button
    And I see the "Password Choice" dialog
    And I enter shielded password:
      | password  | repeatedPassword |
      | Secret123 | Secret123        |
    And I click on "I understand the importance of the password and I will keep it secure." checkbox
    And Print button is no longer disabled
    And I click on the print button
    And I see the "Certificate Generation Complete" dialog
    And I click on "Yes, paper wallet certificate successfully printed and everything is readable and scannable." checkbox
    And Continue button is no longer disabled
    And I click on the continue button
    And I see the "Certificate Password" dialog
    And Shown password should be equal to entered password
    And I click on "I understand that I can not use my certificate without the password and I have stored it safely." checkbox
    And Continue button is no longer disabled
    And I click on the continue button
    And I see the "Verify Certificate" dialog
    And I enter wrong shielded recovery phrase:
      | recoveryPhrase                                                                             |
      | walk output dove result tent stumble train among cabin ripple grab true clean legal asthma |
    And I enter password "Secret123"
    And I should see the following field error message:
      | message                       |
      | global.errors.invalidMnemonic |
    And Checkboxes should be disabled
    And Continue button should be disabled
