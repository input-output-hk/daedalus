@unit
Feature: Spending password validation

  - should contain at least one digit
  - should contain at least one lower case
  - should contain at least one upper case
  - should contain at least 7 characters long
  - should allow passwords in caseless languages like Kanji
  - should enforce the case rules in mixed language passwords

  Scenario Outline: Valid spending passwords
    Given I use the spending password "<PASSWORD>"
    Then the spending password validation should succeed

    Examples:
      | PASSWORD           |
      | Correct1           |
      | 学年別漢字配当表1     |
      | 正确的马电池钉1      |
      | 正确的马电池钉1!     |
      | Mix学年別漢字配1     |
      | Привет1            |
      | mixПривет1         |
      | !№;%:?()_+1Mm      |
      | Mysuperpassword1!  |
      | 正确的马电池钉1>     |
      | 正1$%+#_>          |

  Scenario Outline: Invalid spending passwords
    Given I use the spending password "<PASSWORD>"
    Then the spending password validation should fail

    Examples:
      | PASSWORD       |
      | 2Short         |
      | S p a c 3 s    |
      | ONLY8UPPERCASE |
      | only8lowercase |
      | 学年別漢字配当表  |
      | mix学年別漢字配  |
      | Привет!        |
      | привет1        |
      | mixпривет1     |
      | 1234567        |
      | 123456!        |
      | !№;%:?*()_+1   |
