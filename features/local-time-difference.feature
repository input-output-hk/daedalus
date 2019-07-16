@e2e
Feature: Local Time Difference

  Scenario: Wrong local time
    Given I set the local time difference to be 20 seconds
    And I have selected English language
    And I have accepted "Terms of use"
    Then the system time error overlay should be visible
    And the system time difference should be "20 seconds"

  Scenario: Wrong local time in the first check
    Given I set the local time difference to be 60 seconds
    And I set the local time difference to be 0 seconds
    And I have selected English language
    And I have accepted "Terms of use"
    Then the system time error overlay should be hidden

  Scenario: Wrong local time in the first two checks
    Given I set the local time difference to be 60 seconds
    And I set the local time difference to be 120 seconds
    And I set the local time difference to be 0 seconds
    And I have selected English language
    And I have accepted "Terms of use"
    Then the system time error overlay should be hidden

  Scenario: Wrong local time in all three checks
    Given I set the local time difference to be 60 seconds
    And I set the local time difference to be 120 seconds
    And I set the local time difference to be 180 seconds
    And I have selected English language
    And I have accepted "Terms of use"
    Then the system time error overlay should be visible
    And the system time difference should be "3 minutes"
