@unit
Feature: Generate fileName with timestamp

  Scenario: I don't pass any props and use the default ones
    Given I dont pass any props to the function
    Then the prefix should be "logs"
    And the extention should be "zip"
    And the time should be converted into UTC

  Scenario Outline: I pass the following props
    Given I pass the following props to the function:
    | prefix   | extention   | isUTC   |
    | <prefix> | <extention> | <isUTC> |
    Then the prefix should be "<prefix>"
    And the extention should be "<extention>"
    And the time <isUTC> be converted into UTC

    Examples:
    | prefix     | extention     | isUTC     |
    | testPrefix | docx          | should    |
    |            | pdf           | shouldn't |
