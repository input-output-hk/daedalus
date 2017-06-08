Feature: Toggle Sidebar Submenus

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"
    And I have a wallet with funds
    And I am on the "Genesis wallet" wallet "summary" screen

  Scenario: Hide Sidebar Submenus
    Given the sidebar submenu is visible
    When I click on the sidebar toggle button
    Then the sidebar submenu should be hidden

  Scenario: Show Sidebar Submenus
    Given the sidebar submenu is hidden
    When I click on the sidebar toggle button
    Then the sidebar submenu should be visible
