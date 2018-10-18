Feature: Send Money to Receiver

  Background:
    Given I have completed the basic setup
    And I am on the General Settings "support" screen

  Scenario: Open and close the support window
    Given I click on the support request button
    Then The Support Window should open
    And The Zendesk form should appear
    When I click the cancel button
    Then The window should close
