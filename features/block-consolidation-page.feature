Feature: Display Block Consolidation Page

  Background:
    Given I have completed the basic setup

  Scenario: Open/close the Block Consolidation Page and ensure epoch consolidation data is rendered correctly
    When I toggle the Block Consolidation Status Page
    Then the Block Consolidation Status Page is visible
    And the page accurately renders an explanation of how block consolidation works in file storage
    And the page accurately renders epochs consolidated out of the total in the main blocks graphic
    And the page accurately renders epochs consolidated above the progress bar
    And the page accurately renders the epoch trailing 2 behind the current epoch above the progress bar
    And the page accurately renders the current epoch signifying the max end of the progress bar
    And the page accurately renders the node's sync progress as a percentage below the progress bar
    When I toggle the Block Consolidation Status Page
    Then the Block Consolidation Status Page is hidden

  Scenario: Fetch current epoch from Cardano Explorer and ensure epoch consolidation data is rendered correctly
    When I set the Node Settings Api Request falty
    And I toggle the Block Consolidation Status Page
    Then the Block Consolidation Status Page is visible
    And the page immediately renders an explanation of how block consolidation works in file storage
    And the page immediately renders epochs consolidated out of the total in the main blocks graphic
    And the page hides the node's sync progress as a percentage below the progress bar
    And the page immediately renders the progress bar in loading state
    When the fallback function returns the current epoch
    And the page accurately renders an explanation of how block consolidation works in file storage
    And the page accurately renders epochs consolidated out of the total in the main blocks graphic
    When I toggle the Block Consolidation Status Page
    Then the Block Consolidation Status Page is hidden
