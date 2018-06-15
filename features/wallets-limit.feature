Feature: Wallet Settings

  Background:
    Given I have completed the basic setup
    And I create wallets until I reach the maximum number permitted

  Scenario: User reaches the maximum number of wallets
    Given I click on the add wallet button in the sidebar
    Then I should see maximum number of wallets in the wallets list
    And The buttons in the Add Wallet screen should be disabled
    And I should see a disclaimer saying I have reached the maximum number of wallets

  # Scenario: User deletes one wallet and re-enable its Adding new wallets
  #   Given I delete the wallet "wallet 20"
  #   And I go to the Add wallet screen
  #   Then The buttons should be disabled
  #   And I should see a text saying I have reached the maximum number of wallets


