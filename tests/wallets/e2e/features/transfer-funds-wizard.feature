@e2e @wip
Feature: Transfer funds wizard

  Background:
    Given I have completed the basic setup

  Scenario: Successfully transferring funds from "Daedalus Byron" wallet to "Daedalus Shelley" wallet
    Given I have a "Byron Wallet" byron wallet for transferring funds
    And I have a "Test Wallet" wallet with funds
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And I see initial wallets balance
    And "Byron" wallet badge should be visible in the wallet sidebar
    And "Byron" wallet "Move testnet ada" action should be visible in the top bar notification
    And I click "Byron" wallet top bar notification action
    Then I should see "Transfer ada" wizard
    And I open "Shelley wallet" selection dropdown
    And I select "Shelley Wallet" wallet
    And I click continue button on "Transfer ada" wizard
    Then I should see "Transfer ada" wizard step 2 dialog
    And I enter spending password in "Transfer ada" wizard step 2 dialog:
      | password   |
      | Secret1234 |
    Then "Transfer ada" wizard step 2 dialog continue button should be disabled
    And I click continue button on "Transfer ada" wizard step 2 dialog
    And I see "Transfer ada" wizard step 2 transfer funds button disabled
    Then I should not see "Transfer ada" wizard step 2 wizard dialog anymore
    Then I should see increased shelley wallet balance and 0 ADA in Daedalus Byron wallet

  Scenario: User enters wrong spending password
    Given I have a "Byron Wallet" byron wallet with funds
    And I have a "Test Wallet" wallet with funds
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Byron" wallet badge should be visible in the wallet sidebar
    And "Byron" wallet "Move testnet ada" action should be visible in the top bar notification
    And I click "Byron" wallet top bar notification action
    Then I should see "Transfer ada" wizard
    And I open "Shelley wallet" selection dropdown
    And I select "Shelley Wallet" wallet
    And I click continue button on "Transfer ada" wizard
    Then I should see "Transfer ada" wizard step 2 dialog
    And I enter spending password in "Transfer ada" wizard step 2 dialog:
      | password   |
      | Secret1234Wrong |
    Then "Transfer ada" wizard step 2 dialog continue button should be disabled
    And I click continue button on "Transfer ada" wizard step 2 dialog
    Then I should see the following error messages on transfer wizard step 2 dialog:
      | message                   |
      | api.errors.IncorrectPasswordError |
    And "Transfer ada" wizard step 2 dialog continue button should not be disabled anymore

  Scenario: User enters too short spending password
    Given I have a "Byron Wallet" byron wallet with funds
    And I have a "Test Wallet" wallet with funds
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Byron" wallet badge should be visible in the wallet sidebar
    And "Byron" wallet "Move testnet ada" action should be visible in the top bar notification
    And I click "Byron" wallet top bar notification action
    Then I should see "Transfer ada" wizard
    And I open "Shelley wallet" selection dropdown
    And I select "Shelley Wallet" wallet
    And I click continue button on "Transfer ada" wizard
    Then I should see "Transfer ada" wizard step 2 dialog
    And I enter spending password in "Transfer ada" wizard step 2 dialog:
      | password   |
      | wrong      |
    Then "Transfer ada" wizard step 2 dialog continue button should be disabled
    And I click continue button on "Transfer ada" wizard step 2 dialog
    Then I should see the following error messages on transfer wizard step 2 dialog:
      | message                   |
      | api.errors.IncorrectPasswordError |
    And "Transfer ada" wizard step 2 dialog continue button should not be disabled anymore
