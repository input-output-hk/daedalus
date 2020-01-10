@e2e
Feature: Transfer funds wizard

  Background:
    Given I have completed the basic setup

  Scenario: Balance wallet is shown on the bottom of the list below Rewards wallet in order of creation
    When I restore "Daedalus Balance Wallet" balance wallet with funds
    And I restore "Rewards Wallet" wallet with funds
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
 