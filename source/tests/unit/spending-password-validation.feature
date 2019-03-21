Feature: Spending password validation

  - should contain at least one digit
  - should contain at least one lower case
  - should contain at least one upper case
  - should contain at least 7 characters long
  - should allow passwords in caseless languages like Kanji
  - should enforce the case rules in mixed language passwords

  Scenario Outline:
    Given I use the spending password "<PASSWORD>"
    Then the spending password validation is <EXPECTED_RESULT>

    Examples:
      | PASSWORD       | EXPECTED_RESULT |
      | Correct1       | true            |
      | 学年別漢字配当表1| true            |
      | Mix学年別漢字配1 | true            |
      | Привет1        | true            |
      | mixПривет1     | true            |
      | 2Short         | false           |
      | S p a c 3 s    | false           |
      | ONLY8UPPERCASE | false           |
      | only8lowercase | false           |
      | 学年別漢字配当表 | false           |
      | mix学年別漢字配  | false           |
      | Привет!        | false           |
      | привет1        | false           |
      | mixпривет1     | false           |
