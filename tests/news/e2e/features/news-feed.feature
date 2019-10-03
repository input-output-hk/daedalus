@wip
Feature: News feed

  News feed delivers important information to Daedalus users, like information about network failures, bugs and other
  issues, information about upcoming and recent releases and so on. News are categorised in 4 types:
  incident, alert, announcement and info. Incidents and alerts cover the whole user interface,
  announcements and info are delivered in the news feed part of the user interface.

  Scenario: Bell icon is highlighted when there are unread news

    Given I have Daedalus running
    And there are unread news
    Then the bell icon is highlighted

  Scenario: Bell icon is not highlighted when there are no unread news

    Given I have Daedalus running
    And there are no unread news
    Then the bell icon is not highlighted

  Scenario: No news available

    Given I have Daedalus running
    And there is no news
    Then the bell icon is not highlighted
    When I click on the bell icon
    Then the news feed is opened
    And the news feed contains "no news available" message

  Scenario: Only read news available

    Given I have Daedalus running
    And there are 5 read news
    When I click on the bell icon
    Then the news feed is opened
    And the news feed contains 5 read news

  Scenario: Displaying an incident

    Given I have Daedalus running
    And there is an incident
    Then the incident will cover the screen

  Scenario: Dismissing an alert

    Given I have Daedalus running
    And there are unread alerts
    Then the latest alert will cover the screen
    When I dismiss the alert
    Then the alert I have dismissed becomes read

  Scenario: Reading an announcement

    Given I have Daedalus running
    And there is 1 unread announcement
    When I click on the bell icon
    Then the news feed is opened
    And the news feed contains 1 unread announcement
    When I click on the unread announcement to expand it
    Then the announcement content is shown
    And the announcement is marked as read

  Scenario: Reading an info

    Given I have Daedalus running
    And there is 1 unread info
    When I click on the bell icon
    Then the news feed is opened
    And the news feed contains 1 unread info
    When I click on the unread info to expand it
    Then the info content is shown
    And the info is marked as read
    And the bell icon is not highlighted

  Scenario: News unavailable

    Given I have Daedalus running
    And news feed server is unreachable
    When I click on the bell icon
    Then the news feed is opened
    And the news feed contains "news unavailable message"
