@e2e @skip
# @API TODO - Rust node stores block data in a different directory so this has to be handled first
Feature: Display Block Consolidation Page

  Background:
    Given I have completed the basic setup

  Scenario: Open/close the Block Consolidation Page and ensure epoch consolidation data is rendered correctly
    When I open the Block Consolidation Status Dialog
    Then the Block Consolidation Status Page is visible
    And the page accurately renders the follow explanation of how block consolidation works in file storage:
      | message                               |
      | blockConsolidationStatus.description2 |
    And the page accurately renders epochs consolidated out of the total in the main blocks graphic
    And the page accurately renders epochs consolidated above the progress bar:
      | message                                     |
      | blockConsolidationStatus.epochsConsolidated |
    And the page accurately renders the epoch trailing 2 behind the current epoch above the progress bar:
      | message                        |
      | blockConsolidationStatus.epoch |
    And the page accurately renders the current epoch signifying the max end of the progress bar:
      | message                        |
      | blockConsolidationStatus.epoch |
    And the page accurately renders the node's sync progress as a percentage below the progress bar:
      | message                        |
      | blockConsolidationStatus.synced |
    When I close the Block Consolidation Status Dialog
    Then the Block Consolidation Status Page is hidden

  Scenario: Fetch current epoch from Cardano Explorer and ensure epoch consolidation data is rendered correctly
    When I set the Node Setting Api Request to return faulty response
    And I open the Block Consolidation Status Dialog
    Then the Block Consolidation Status Page is visible
    And the page hides the node's sync progress as a percentage below the progress bar
    And the page renders the progress bar in loading state
    When the fallback function returns the current epoch
    And the page accurately renders the follow explanation of how block consolidation works in file storage:
      | message                               |
      | blockConsolidationStatus.description2 |
    And the page accurately renders epochs consolidated out of the total in the main blocks graphic
    And the page accurately renders epochs consolidated above the progress bar:
      | message                                     |
      | blockConsolidationStatus.epochsConsolidated |
    When I close the Block Consolidation Status Dialog
    Then the Block Consolidation Status Page is hidden
