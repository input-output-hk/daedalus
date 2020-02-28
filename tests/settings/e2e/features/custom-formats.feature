@e2e @watch
Feature: Custom number, date and time formats

  Background:
    Given I have chosen custom number, date and time formats
    And I have completed the basic setup

  Scenario: Initial profile setup is working as expected
    Given I am on the General Settings "general" screen
    Then I should see the correct chosen options

  Scenario: Changing number/date/time format on the "Settings > General" screen works as expected
    Given I am on the General Settings "general" screen
    And I have chosen new custom number, date and time formats
    Then I should see the correct new chosen options


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
