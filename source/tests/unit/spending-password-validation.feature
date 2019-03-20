Feature: is valid spending password

  - should contain at least one digit: (?=.*\d)
  - should contain at least one lower case: (?=.*[а-я])
  - should contain at least one upper case: (?=.*[А-Я])
  - should contain at least 7 characters long: .{7,}

  Scenario Outline:
    Given I use the password "<PASSWORD>"
    Then the password validation should be <EXPECTED_RESULT>

    Examples:
      | PASSWORD       | EXPECTED_RESULT |
      | Correct1       | true            |
      | 学年別漢字配当表1| true            |
      | Привет1        | true            |
      | 2Short         | false           |
      | S p a c 3 s    | false           |
      | ONLY8UPPERCASE | false           |
      | only8lowercase | false           |
      | 学年別漢字配当表 | false           |
      | Привет!        | false           |
