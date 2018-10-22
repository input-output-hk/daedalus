Feature: Send Money to Receiver

  Background:
    Given I have completed the basic setup
    And I am on the General Settings "support" screen
    And I open the Support Request window

  Scenario: The form is automatically filled
    Then The following fields are filled:
      | field            |
      | product          |
      | supportLanguage  |
      # | productVersion   |

  Scenario: Logs attached
    Then The compressed logs zip file was attached

  Scenario: Cancel button to close the support window
    Given I click the cancel button
    Then The window should close
