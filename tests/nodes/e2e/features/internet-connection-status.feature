@e2e
Feature: Internet Connection Status

  Scenario: No Internet connection on Daedalus start
    Given I set Internet Connection to: "offline"
    Then I should see Internet Connection status dialog

  Scenario: Daedalus loses Internet connection when is up and running
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |
    And I am on the "Test wallet" wallet "settings" screen
    And I set Internet Connection to: "offline"
    Then I should see Internet Connection status dialog


  Scenario: Daedalus loses Internet connection when is up and running
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |
    And I am on the "Test wallet" wallet "settings" screen
    And I set Internet Connection to: "offline"
    Then I should see Internet Connection status dialog
    And I click on "Check again" button
    And I see loading spinner
    Then I should not see the Internet connection status dialog anymore