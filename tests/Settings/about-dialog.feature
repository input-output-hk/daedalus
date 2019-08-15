@e2e
Feature: About Dialog

  Background:
    Given I have completed the basic setup

  Scenario: Open/close the About dialog and compare its version to the package.json
    Given I open the About dialog
    Then the About dialog is visible
    And the About dialog and package.json show the same Daedalus version
    When I close the About dialog
    Then the About dialog is hidden
