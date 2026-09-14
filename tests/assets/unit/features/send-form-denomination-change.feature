@unit
Feature: Send Form Denomination Change

  The amount a user signs for a token is the digits on screen with their
  separators removed, and nothing records which denomination they were typed in.
  The decimal places for a token now arrive from the metadata cache while the
  form is open, so a row pins the denomination it was opened in and defends it.

  Scenario: A resolution arrives while an amount is on screen
    Given a send form row for a token whose decimal places are unknown
    And I have entered "1500000" in that row
    Then the form would submit "1500000" natural units for that row
    When the cache resolves that token to 6 decimal places
    Then that row's amount field is empty
    And that row reports a denomination change
    And the form would not submit "1500000" natural units for that row
    And the form would not submit "1500000000000" natural units for that row
    And that row is denominated in 6 decimal places

  Scenario: A resolution arrives while the field is empty
    Given a send form row for a token whose decimal places are unknown
    When the cache resolves that token to 6 decimal places
    Then that row reports no denomination change
    And that row's amount field is empty
    And that row is denominated in 6 decimal places

  Scenario: A cached value changes on re-read while an amount is on screen
    Given a send form row for a token with 6 decimal places
    And I have entered "1.500000" in that row
    Then the form would submit "1500000" natural units for that row
    When the cache resolves that token to 2 decimal places
    Then that row's amount field is empty
    And that row reports a denomination change
    And that row is denominated in 2 decimal places

  Scenario: Nothing is disturbed when a re-read finds the same value
    Given a send form row for a token with 6 decimal places
    And I have entered "1.500000" in that row
    When the cache resolves that token to 6 decimal places
    Then that row reports no denomination change
    And the form would submit "1500000" natural units for that row
