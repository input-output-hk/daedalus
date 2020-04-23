@unit
Feature: Spending password validation

  - should contain at least one digit
  - should contain at least one lower case
  - should contain at least one upper case
  - should contain at least 10 characters long
  - should allow passwords in caseless languages like Kanji
  - should enforce the case rules in mixed language passwords

  Scenario Outline: Valid spending passwords
    Given I use the spending password "<PASSWORD>"
    Then the spending password validation should succeed

    Examples:
      | PASSWORD           |
      | Correct123         |
      | 学年別漢字配当表12    |
      | 正确的马电池钉123     |
      | 正确的马电池钉12!     |
      | Mix学年別漢字配1     |
      | Привет1234         |
      | mixПривет1         |
      | !№;%:?()_+1Mm      |
      | Mysuperpassword1!  |
      | 正确的马电池钉12>     |
      | 正123$%+#_>         |

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
      | Привет!#$%     |
      | привет1234     |
      | mixпривет1     |
      | 1234567890     |
      | 123456789!     |
      | !№;%:?*()_+1   |
