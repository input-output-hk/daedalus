@e2e
Feature: Stake-pool server settings

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test Wallet |

  Scenario: Navigating to Stake-pool settings screen and selecting custom stake-pool server
    Given I am on the General Settings "stake-pools" screen
    When custom server is the default option
    And The smash server input textBox is visible
    And And I enter a custom server URL as custom server option
    And I clicked outside of the input-box to change focus
    And I click the stake-pool custom server input box submit button
    And I see the "Your changes have been saved" success message
    And I am on the Delegation "stake-pools" screen
    Then The custom server I chose earlier is visible on stake-pool screen above stake-pool list and is clickable
    And I am brought back to the stake-pool server settings screen

  Scenario: Verify stake-pool custom server text box is visible and error message verification
    Given I am on the General Settings "stake-pools" screen
    When I open stake pool server dropdown
    And I select custom server option
    Then The smash server input textBox is visible
    And I enter invalid url "www.test" in to the custom server input-box
    Then Stake-pool custom input box error message "The URL should start with" is displayed
    And I delete values in smash server custom url input box
    And I enter invalid url "https://test.com/path" in to the custom server input-box
    Then Stake-pool custom input box error message "protocol and hostname" is displayed
    And I delete values in smash server custom url input box
    And I enter invalid server "https://www.google.ie" containing https
    And I clicked outside of the input-box to change focus
    And I click the stake-pool custom server input box submit button
    Then I see the "This url is not a valid SMASH server" error message displayed
