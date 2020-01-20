@e2e
Feature: Wallet - UTXOs

  Background:
    Given I have completed the basic setup

  Scenario: Chart visible. Page title and description
    Given I have a "Test Wallet" rewards wallet with funds
    When I am on the "Test Wallet" wallet "utxo" screen
    Then the "title" element renders the following text:
      | message                     |
      | wallet.settings.utxos.title |
    And the page description displays the correct wallet and UTXOs amount
    And the UTXOs chart is visible

  Scenario: The wallet does not contain any UTxOs
    Given I have the following "Rewards" wallets:
      | name      |
      | NewWallet |
    When I am on the "NewWallet" wallet "utxo" screen
    Then the "title" element renders the following text:
      | message                     |
      | wallet.settings.utxos.title |
    Then the "description" element renders the following text:
      | message                           |
      | wallet.settings.utxos.emptyWallet |
    And the UTXOs chart is hidden
