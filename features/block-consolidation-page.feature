Feature: Display Block Consolidation Page

  Background:
    Given I have completed the basic setup
  
  Scenario: Open/close the Block Consolidation Page and ensure epoch consolidation data is rendered correctly
    When I open the Block Consolidation Status Page
    Then the Block Consolidation Status Page is visible
    And the page accurately renders how many epochs are consolidated out of the total
    When I close the Block Consolidation Status Page
    Then the Block Consolidation Status Page is hidden
