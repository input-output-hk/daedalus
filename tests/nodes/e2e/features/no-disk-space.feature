@e2e
Feature: No Disk Space Overlay

  Background:
    Given I have completed the basic setup

  Scenario: No disk space overlay is not shown when there is enough available disk space
    Given I set the required space to 1 KB
    And I check the disk space
    Then The No Disk Space overlay should be hidden

  Scenario: No disk space overlay is shown when there is not enough available disk space and hidden once more disk space becomes available
    Given I set the required space to 100 TB
    And I check the disk space
    Then The No Disk Space overlay should be visible
    And I set the required space to 1 KB
    Then The No Disk Space overlay should be hidden
