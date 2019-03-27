@ui
Feature: Local Time Difference

  Scenario: Wrong local time
    Given I set wrong local time difference
    And I have selected English language
    And I have accepted "Terms of use"
    Then I should see system time error overlay
