Feature: Login

  Background:
    Given I have an account
    And I have a wallet

  Scenario: Successful Login
    Given I am on the login screen
    When I submit login form with the following inputs:
    | email              | password |
    | satoshi@gmail.com  | password |
    Then I should be on some wallet page
    And I dont see the login window anymore
