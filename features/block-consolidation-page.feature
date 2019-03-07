Feature: Display Block Consolidation Page

  Background:
    Given I have completed the basic setup
  
  Scenario: Open/close the Block Consolidation Page and ensure epoch consolidation data is rendered correctly
    Given I set the current epoch and number of epochs consolidated
    And I open the Block Consolidation Page
    Then the Block Consolidation Page is visible
    And I should see the Block Consolidation Page display "Blocks for the current epoch (30) and the previous epoch (29) are stored as one file per block."
    # Todo: Add more steps to test DOM elements rendered in the BlockConsolidationStatus.js component
    When I close the Block Consolidation Page
    Then the Block Consolidation Page is no longer visible
