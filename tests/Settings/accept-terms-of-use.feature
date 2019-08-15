@e2e
Feature: Accept "Terms of use"

  Background:
    Given I didnt accept "Terms of use"
    And I have selected English language

  Scenario: User accepts "Terms of use"
    Given I am on the "Terms of use" screen
    And I click on "I agree with terms of use" checkbox
    When I submit the "Terms of use" form
    Then I should not see the "Terms of use" screen anymore
    And I should have "Terms of use" accepted
