@e2e
Feature: Node Restart

  Background:
    Given I have completed the basic setup
    And cardano-node is running

  Scenario Outline: I restart Cardano Node from the "Diagnostics" screen for the <ATTEMPT>. time
    Given I open the "Diagnostic" screen
    Then I should see the Cardano Node state is "Running"
    When I click on the "Restart Cardano Node" button
    And I should see the main UI

    Examples:
    | ATTEMPT |
    | 1       |
    | 2       |
    | 3       |
    | 4       |
    | 5       |
