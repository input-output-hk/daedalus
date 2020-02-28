@e2e @watch
Feature: Custom number, date and time formats

  Background:
    Given I have chosen the following custom formats:
      | PARAM  | VALUE                |
      | number | 8.638.301.639,283542 |
      | date   | dd/mm/yyyy           |
      | time   | 14:00                |
    And I have completed the basic setup

  Scenario: Initial profile setup is working as expected
    Given I am on the General Settings "general" screen
    Then I should see the following chosen options:
      | PARAM  | VALUE      |
      | number | number-2   |
      | date   | DD/MM/YYYY |
      | time   | HH:mm:ss   |

  Scenario: Changing number/date/time format on the "Settings > General" screen works as expected
    Given I am on the General Settings "general" screen
    Given I have chosen the following custom formats:
      | PARAM  | VALUE                |
      | number | 8 638 301 639.283542 |
      | date   | yyyy/mm/dd           |
      | time   | 02:00 PM             |
    Then I should see the following chosen options:
      | PARAM  | VALUE      |
      | number | number-3   |
      | date   | YYYY/MM/DD |
      | time   | hh:mm:ss A |


  # Scenario: Dates are displayed in the correct user preference format

  # Scenario: Time is displayed in the correct user preference format

  # Scenario: Numbers are displayed in the correct user preference format
  # # (including wallet sidebar wallet balance display)

  # Scenario: Users can create transactions regardless of their number format preference
  # # (including testing amounts bigger than 1 million ada)


# # DATE

# AlertsOverlay
# IncidentOverlay
# NewsFeed
# NewsItem
# InitialSettings
# GeneralSettings
# WalletTransactionsList
# ProfileSettingsForm
# NewsFeedContainer
# NewsOverlayContainer
# WalletSummaryPage
# FilterDialogContainer
# addressPDFGenerator
# profileSettings

# # TIME
# Transaction

# # NUMBER
# WalletSendForm
