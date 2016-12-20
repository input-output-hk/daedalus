Feature: Switching Language
  In order to use Daedalus in my native language
  As a User
  I want to be able to switch language in my profile settings

  Background:
    Given I have an account
    And I have a wallet
    And I am logged in

  Scenario: Switching from English to German
    Given I am on the settings screen
    And My current language is "en-EN"
    When I select "German" from the language dropdown on the settings page
    Then My current language should be "de-DE"
