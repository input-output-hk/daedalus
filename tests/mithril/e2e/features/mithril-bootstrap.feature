@e2e @mithrilBootstrap
Feature: Mithril bootstrap

  Background:
    Given I have completed the basic setup

  Scenario: Accept Mithril bootstrap and show the progress view
    Given Mithril bootstrap is seeded on the decision view
    When I accept Mithril bootstrap
    Then I should see the Mithril progress view

  Scenario: Decline Mithril bootstrap and hide the overlay
    Given Mithril bootstrap is seeded on the decision view
    When I decline Mithril bootstrap
    Then the Mithril bootstrap overlay should be hidden

  Scenario: Advance progress stages through preparing, downloading, and finalizing
    Given Mithril bootstrap is seeded on the progress view for "preparing"
    Then the "preparing" Mithril step should be active
    When Mithril bootstrap advances to the "downloading" stage
    Then the "downloading" Mithril step should be active
    When Mithril bootstrap advances to the "finalizing" stage
    Then the "finalizing" Mithril step should be active

  Scenario: Cancel bootstrap and return to the chain storage view
    Given Mithril bootstrap is seeded on the progress view for "downloading"
    When I cancel Mithril bootstrap
    Then I should see the Mithril chain storage view

  Scenario Outline: Show staged Mithril errors
    Given Mithril bootstrap is seeded on the "<stage>" error view
    Then I should see the "<stage>" Mithril error heading

    Examples:
      | stage      |
      | download   |
      | verify     |
      | convert    |
      | node-start |

  Scenario: Change the chain storage location
    Given Mithril bootstrap is seeded on the chain storage view
    When I choose a new Mithril chain storage location
    And I continue from the Mithril chain storage view
    Then the Mithril decision view should show the selected chain storage path

  Scenario: Reset the chain storage location to default
    Given Mithril bootstrap is seeded on the chain storage view with custom storage "/tmp/daedalus/mithril-custom-chain"
    When I reset the Mithril chain storage location to default
    And I continue from the Mithril chain storage view
    Then the Mithril decision view should show the default chain storage path

  Scenario: Validate insufficient space in chain storage selection
    Given Mithril bootstrap is seeded on the chain storage view
    And the Mithril chain storage validation fails for the selected path because of insufficient space
    When I choose a new Mithril chain storage location
    Then I should see the Mithril insufficient space validation message

  Scenario: Disable chain storage controls during busy state
    Given Mithril bootstrap is seeded on the chain storage view with busy storage controls
    Then the Mithril chain storage select button should be disabled
    And the Mithril chain storage continue button should be disabled
    And the Mithril chain storage reset button should be disabled