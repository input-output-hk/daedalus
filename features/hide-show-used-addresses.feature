Feature: Hide/show used addresses

  The wallet receive screen has a switcher to hide used addresses

  Background:
    Given I have completed the basic setup
    And I have a "Genesis wallet" with funds
    And I am on the "Genesis wallet" wallet "receive" screen

  Scenario: No click
    Given I generate 6 addresses
    Then I should see 6 addresses

