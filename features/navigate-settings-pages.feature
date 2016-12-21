Feature: Navigate Settings Pages
  In order to adjust my settings
  As a User
  I want to be able to navigate between the settings pages

  Background:
    Given I have an account
    And I have a wallet
    And I am logged in

  Scenario Outline: Switching Between Settings Pages
    Given I am on the <FROM> settings screen
    When I click on the <TO> settings menu item
    Then I should be on the <TO> settings screen

    Examples:
    | FROM       | TO         |
    | profile    | termsOfUse |
    | termsOfUse | profile    |
