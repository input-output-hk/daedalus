Feature: is valid spending password
  Validation rules:
  - should contain at least one digit: (?=.*\d)
  - should contain at least one lower case: (?=.*[а-я])
  - should contain at least one upper case: (?=.*[А-Я])
  - should contain at least 7 characters long: .{7,}

  @watch
  Scenario Outline:
    Given I use the password "<PASSWORD>"
    When I apply our password validation rules
    Then it should give me "<EXPECTED_RESULT>"

    Examples:
      | PASSWORD | EXPECTED_RESULT |
      | first    | false           |
