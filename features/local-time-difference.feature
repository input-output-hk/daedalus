Feature: Local Time Difference

Background:
  Given I have selected English language
  And I have accepted "Terms of use"
  And I agree to send logs to remote server

  @skip
  Scenario: Wrong local time
    And I see the add wallet dialog
    And I set wrong local time
    Then I should see system time error overlay
