Feature: Editing profile settings

  Background:
    Given I have an account
    And I have a wallet
    And I am logged in

  Scenario: Updating the Name on Profile Settings page
    Given I am on the profile settings screen
    When I click on the name input field on profile settings page
    Then Name input field should become enabled
    When I enter "New Name" into the name input field
    And I press the "Enter" key
    Then Name input field should become disabled
    And The value of name input field should be "New Name"
