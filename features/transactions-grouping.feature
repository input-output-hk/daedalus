Feature: Transactions Grouping
  In order to see clearly when transactions have been executed
  As a User
  I want to see them grouped by date

  Background:
    Given I have completed the basic setup
    And I have a "Genesis wallet" with funds
    And I have the following wallets:
      | name       |
      | TestWallet |

  Scenario: Transactions are Grouped by Date in the transaction screen
    Given I have made the following transactions:
      | sender         | receiver   | amount |
      | Genesis wallet | TestWallet | 1      |
      | Genesis wallet | TestWallet | 2      |
    When I am on the "TestWallet" wallet "transactions" screen
    Then The transactions should be ordered from newest to oldest
    Then The transactions should be grouped by their date

  Scenario: Transactions are Grouped by Date in the transaction screen
    Given I have made the following transactions:
      | sender         | receiver   | amount |
      | Genesis wallet | TestWallet | 1      |
      | Genesis wallet | TestWallet | 2      |
    When I am on the "TestWallet" wallet "summary" screen
    Then The transactions should be ordered from newest to oldest
    Then The transactions should be grouped by their date

