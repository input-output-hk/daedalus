@e2e @shelley
Feature: Custom number, date and time formats

  Background:
    Given I choose the following custom formats:
      | param  | value                |
      | number | 8.638.301.639,283542 |
      | date   | dd/mm/yyyy           |
      | time   | 14:00                |
    And I have completed the basic setup

  Scenario: Initial profile setup is working as expected
    Given I am on the General Settings "general" screen
    Then I should see the following chosen options:
      | param  | value      |
      | number | number-2   |
      | date   | DD/MM/YYYY |
      | time   | HH:mm:ss   |

  Scenario: Changing number/date/time format on the "Settings > General" screen works as expected
    Given I am on the General Settings "general" screen
    And I choose the following custom formats:
      | param  | value                |
      | number | 8 638 301 639.283542 |
      | date   | yyyy/mm/dd           |
      | time   | 02:00 PM             |
    Then I should see the following chosen options:
      | param  | value      |
      | number | number-3   |
      | date   | YYYY/MM/DD |
      | time   | hh:mm:ss A |

  Scenario: Transactions list displays the correct user date preference format
    Given I have a "Test Wallet" wallet with funds
    And I have the following "Rewards" wallets:
      | name          |
      | Target Wallet |
    And I am on the "Test Wallet" wallet "transactions" screen
    And I have made the following transactions:
      | source      | destination   | amount |
      | Test Wallet | Target Wallet | 100000 |
    Then the "transaction" should display the following custom formats:
      | param  | value      |
      | number | .,         |
      | date   | DD/MM/YYYY |
      | time   | HH:mm:ss   |
    And I have changed the following custom formats:
      | param  | value      |
      | number | number-1   |
      | date   | YYYY/MM/DD |
      | time   | hh:mm:ss A |
    Then the "transaction" should display the following custom formats:
      | param  | value      |
      | number | ,.         |
      | date   | YYYY/MM/DD |
      | time   | hh:mm:ss A |

  Scenario: Transactions list displays the correct user date preference format
    Given I have a "Test Wallet" wallet with funds
    And I have the following "Rewards" wallets:
      | name          |
      | Target Wallet |
    And I am on the "Test Wallet" wallet "transactions" screen
    And I have made the following transactions:
      | source      | destination   | amount |
      | Test Wallet | Target Wallet | 1      |
    When I open the transactions filter
    And I choose the first time filter option
    And I enter the following filter values:
      | param      | value |
      | fromAmount | 12,34 |
      | toAmount   | 12.34 |
    Then the "transaction filter" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |
    And I should see the following filter values:
      | param      | value        |
      | fromAmount | 12,340000    |
      | toAmount   | 1.234,000000 |

  Scenario: Newsfeed alert displays the correct user date preference format
    Given there is 1 unread alert
    Then the "alert" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |

  Scenario: Newsfeed incident displays the correct user date preference format
    Given there is an incident
    Then the "incident" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |

  Scenario: Newsfeed announcement displays the correct user date preference format
    Given there is 1 unread announcement
    When I open the newsfeed
    Then the "announcement" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |

  Scenario: Newsfeed info displays the correct user date preference format
    Given there is 1 unread info
    When I open the newsfeed
    Then the "info" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |

  Scenario: Sidebar wallets display the correct user number preference format
    Given I have a "Test Wallet" wallet with funds
    And I have the following "Rewards" wallets:
      | name          |
      | Target Wallet |
    And I am on the "Target Wallet" wallet "transactions" screen
    And I have made the following transactions:
      | source      | destination   | amount |
      | Test Wallet | Target Wallet | 123456 |
    When the "Target Wallet" wallet has received the transaction amount
    Then the "Target Wallet" should display the "number" of value "123,4K ADA"

  Scenario: Users can create transactions regardless of their number format preference
    Given I have a "Test Wallet" wallet with funds
    And I have the following "Rewards" wallets:
      | name          |
      | Target Wallet |
    And I am on the "Test Wallet" wallet "send" screen
    When I fill out the send form with a transaction to "Target Wallet" wallet:
      | amount   |
      | 123456789,123456 |
    Then the "send form" should display the "number" of value "123.456.789,123456"
    And the "send form" should display the following custom formats:
      | param  | value    |
      | number | ., |
    And I am on the "Test Wallet" wallet "summary" screen
    And I have changed the following custom formats:
      | param  | value    |
      | number | number-1 |
    And I am on the "Test Wallet" wallet "send" screen
    When I fill out the send form with a transaction to "Target Wallet" wallet:
      | amount           |
      | 123456789.123456 |
    Then the "send form" should display the "number" of value "123,456,789.123456"
    And the "send form" should display the following custom formats:
      | param  | value            |
      | number | ,. |
    And I am on the "Test Wallet" wallet "summary" screen
    And I have changed the following custom formats:
      | param  | value    |
      | number | number-3 |
    And I am on the "Test Wallet" wallet "send" screen
    When I fill out the send form with a transaction to "Target Wallet" wallet:
      | amount           |
      | 123456789.123456 |
    Then the "send form" should display the "number" of value "123 456 789.123456"
    And the "send form" should display the following custom formats:
      | param  | value            |
      | number | . |
