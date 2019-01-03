Feature: No Disk Space Overlay

  Background:
    Given I have completed the basic setup

  Scenario: I set the required space to 1 KB
    Given I set the required space to 1 KB
    And I check the disk space
    Then The No Disk Space overlay should be hidden

  Scenario: I set the required space to 100 TB
    Given I set the required space to 100 TB
    And I check the disk space
    Then The No Disk Space overlay should be visible
    And After 10 seconds
    Then The No Disk Space overlay should be hidden
