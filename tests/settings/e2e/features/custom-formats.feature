@e2e
Feature: Custom number, date and time formats

  Background:
    Given I have chosen the following custom formats:
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
    Given I have chosen the following custom formats:
      | param  | value                |
      | number | 8 638 301 639.283542 |
      | date   | yyyy/mm/dd           |
      | time   | 02:00 PM             |
    Then I should see the following chosen options:
      | param  | value      |
      | number | number-3   |
      | date   | YYYY/MM/DD |
      | time   | hh:mm:ss A |

  Scenario: Newsfeed alert displays the correct user date preference format
    Given there is 1 unread alert
    Then The "alert" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |

  Scenario: Newsfeed incident displays the correct user date preference format
    Given there is an incident
    Then The "incident" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |

  Scenario: Newsfeed announcement displays the correct user date preference format
    Given there is 1 unread announcement
    When I open the newsfeed
    Then The "announcement" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |

  Scenario: Newsfeed info displays the correct user date preference format
    Given there is 1 unread info
    When I open the newsfeed
    Then The "info" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |

  Scenario: Transactions list displays the correct user date preference format
    Given I have a "Test Wallet" rewards wallet with funds
    And I have the following "Rewards" wallets:
      | name          |
      | Target Wallet |
    And I am on the "Test Wallet" wallet "transactions" screen
    And I have made the following transactions:
      | source      | destination   | amount |
      | Test Wallet | Target Wallet | 1      |
    Then The "transaction" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |
      | time  | HH:mm:ss   |

  Scenario: Transactions list displays the correct user date preference format
    Given I have a "Test Wallet" rewards wallet with funds
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
    Then The "transaction filter" should display the following custom formats:
      | param | value      |
      | date  | DD/MM/YYYY |
    And I should see the following filter values:
      | param      | value        |
      | fromAmount | 12,340000    |
      | toAmount   | 1.234,000000 |


  # Dates are displayed in the correct user preference format

  # Scenario: Time is displayed in the correct user preference format

  # Scenario: Numbers are displayed in the correct user preference format
  # # (including wallet sidebar wallet balance display)

  # Scenario: Users can create transactions regardless of their number format preference
  # # (including testing amounts bigger than 1 million ada)


# # DATE

# InitialSettings
# GeneralSettings
# ProfileSettingsForm
# FilterDialogContainer
# addressPDFGenerator
# profileSettings
# . NewsFeedContainer
# . WalletSummaryPage
# . NewsOverlayContainer
# . WalletTransactionsList
# . AlertsOverlay
# . IncidentOverlay
# . NewsFeed
# . NewsItem

# # TIME
# . Transaction

# # NUMBER
# WalletSendForm
