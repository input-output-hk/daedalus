Feature: Display Block Consolidation Page

  Background:
    Given I have completed the basic setup
  
  Scenario: Open/close the Block Consolidation Page and ensure epoch consolidation data is rendered correctly
    When I open the Block Consolidation Status Page
    Then the Block Consolidation Status Page is visible
    And the page accurately renders an explanation of how block consolidation works in file storage
    And the page accurately renders epochs consolidated out of the total in the main blocks graphic
    And the page accurately renders epochs consolidated above the progress bar
    And the page accurately renders the epoch trailing 2 behind the current epoch above the progress bar
    And the page accurately renders the current epoch signifying the max end of the progress bar
    And the page accurately renders the node's sync progress as a percentage below the progress bar
    When I close the Block Consolidation Status Page
    Then the Block Consolidation Status Page is hidden
