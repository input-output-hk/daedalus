@e2e
Feature: Transfer funds wizard

  Background:
    Given I have completed the basic setup

  Scenario: Successfully transfering funds from "Daedalus Balance" wallet to "Daedalus Rewards" wallet
    Given I have a "Balance Wallet" balance wallet for transfering funds
    And I have a "Test Wallet" rewards wallet with funds
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And I see initial wallets balance
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification
    And I click "Balance" wallet top bar notification action
    Then I should see "Transfer ada" wizard
    And I open "Rewards wallet" selection dropdown
    And I select "Rewards Wallet" wallet
    And I click continue button on "Transfer ada" wizard
    Then I should see "Transfer ada" wizard step 2 dialog
    And I enter spending password in "Transfer ada" wizard step 2 dialog:
      | password   |
      | Secret1234 |
    Then "Transfer ada" wizard step 2 dialog continue button should be disabled
    And I click continue button on "Transfer ada" wizard step 2 dialog
    And I see "Transfer ada" wizard step 2 transfer funds button disabled and spinner
    Then I should not see "Transfer ada" wizard step 2 wizard dialog anymore
    Then I should see increased rewards wallet balance and 0 ADA in Daedalus Balance wallet

  Scenario: User enters wrong spending password
    Given I have a "Balance Wallet" balance wallet with funds
    And I have a "Test Wallet" rewards wallet with funds
    And I should see the restore status notification while restore is running
    And I should not see the restore status notification once restore is finished
    And "Balance" wallet badge should be visible in the wallet sidebar
    And "Balance" wallet "Move testnet ada" action should be visible in the top bar notification
    And I click "Balance" wallet top bar notification action
    Then I should see "Transfer ada" wizard
    And I open "Rewards wallet" selection dropdown
    And I select "Rewards Wallet" wallet
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
