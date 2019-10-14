@e2e
Feature: Toggle Sidebar Submenus

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |

  Scenario: Hide Sidebar Submenus
    Given the sidebar submenu is visible
    When I click on the sidebar toggle button
    Then the sidebar submenu should be hidden

  Scenario: Show Sidebar Submenus
    Given the sidebar submenu is hidden
    When I click on the sidebar toggle button
    Then the sidebar submenu should be visible
