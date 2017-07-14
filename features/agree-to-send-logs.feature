Feature: Accept "Send logs to server"

  Background:
    Given I didnt choose send logs option
    And I have selected English language
    And I have accepted "Terms of use"

  Scenario: User chooses to send logs to the server
    Given I am on the "Send logs choice" screen
    And I click on "Continue" button
    Then I should not see the "Send logs choice" screen anymore
    And I should have "Send logs" accepted
