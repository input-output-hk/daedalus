@e2e @skip
# @API TODO - we don't have API endpoint for app update check
Feature: Manual Software Update Overlay

	Background:
    Given I have completed the basic setup

  @reconnectApp
  Scenario: Daedalus is unable to connect while there is a new version available
    Given There is a newer application version available
    And Daedalus is stuck in connecting state
    And I should see the "Manual Update" overlay
    Then The overlay should accurately display the version info
