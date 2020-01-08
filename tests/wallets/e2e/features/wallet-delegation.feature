@e2e @watch
Feature: Wallet Delegation

  Background:
    Given I have completed the basic setup

  Scenario: "Create rewards wallet" notification when no Rewards wallets
    Given I am on the Delegation "delegation-center" screen
    Then I should see a "Create rewards wallet" notification

  Scenario: Only Rewards wallets are listed on the "Delegation center" screen
    I have the following wallets
    Given I am on the Delegation "delegation-center" screen
