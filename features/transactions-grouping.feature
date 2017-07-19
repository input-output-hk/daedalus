# TODO: Fix this test case!
@skip
Feature: Transactions Grouping
  In order to see clearly when transactions have been executed
  As a User
  I want to see them grouped by date

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"
    And I agree to send logs to remote server
    And I have a wallet

  Scenario: Transactions are Grouped by Date
    Given I made the following transactions with my wallet:
      | title  | date       |
      | First  | 2016-01-01 |
      | Second | 2016-01-02 |
      | Third  | 2016-01-03 |
    When I am on the wallet summary screen
    Then I should see the transactions grouped by their date
