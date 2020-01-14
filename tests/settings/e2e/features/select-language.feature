@e2e
Feature: Select Language

  Scenario: User Selects Language
    Given I dont have a language set
    And I am on the language selection screen
    And I open language selection dropdown
    And I select Japanese language
    When I submit the language selection form
    Then I should not see the language selection screen anymore
    And I should have Japanese language set
