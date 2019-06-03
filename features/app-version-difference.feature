Feature: App Version Difference Overlay

	Background:
    Given I have completed the basic setup
  
  Scenario: New version available overlay
    Given I set app in connecting state
    And I check for available app updates
    And I get available app version greater than current
    Then I should see Manual Update overlay
 