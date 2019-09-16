@wip
Feature: News feed

  News feed delivers important information to Daedalus users, like information about network failures, bugs and other
  issues, information about upcoming and recent releases and so on. News are categorised in 4 types:
  incident, alert, announcement and info. Incidents and alerts cover the whole user interface,
  announcements and info are delivered in the news feed part of the user interface.

  Scenario: No news available

    Given I have Daedalus running
    And there is no news
    Then the bell icon is not highlighted
    And the news feed is empty
    And the no news available message is shown

  Scenario: Only read news available

    Given I have Daedalus running
    And there are 5 read news
    Then the bell icon is not highlighted
    And the news feed contains 5 read news

  Scenario: Displaying an incident

    Given I have Daedalus running
    And there is an incident
    Then the incident is covering the screen

  Scenario: Dismissing an alert

    Given I have Daedalus running
    And there is 1 unread alert
    Then the alert is covering the screen
    When I dismiss the alert
    And there are no unread alerts
    And the bell icon is not highlighted

  Scenario: Reading an alert

    Given I have Daedalus running
    And there is 1 unread alert
    Then the bell icon is highlighted
    And the news feed contains 1 unread alert
    When I click on the unread alert to expand it
    Then the alert content is covering the screen
    When I dismiss the alert
    Then the alert is marked as read
    And the bell icon is not highlighted

  Scenario: Reading an announcement

    Given I have Daedalus running
    And there is 1 unread announcement
    Then the bell icon is highlighted
    And the news feed contains 1 unread announcement
    When I click on the unread announcement to expand it
    Then the announcement content is shown
    And the announcement is marked as read
    And the bell icon is not highlighted

  Scenario: Reading an info

    Given I have Daedalus running
    And there is 1 unread info
    Then the bell icon is highlighted
    And the news feed contains 1 unread info
    When I click on the unread info to expand it
    Then the info content is shown
    And the info is marked as read
    And the bell icon is not highlighted

  Scenario: News unavailable

    Given I have Daedalus running
    And news feed server is unreachable
    Then the bell icon is not highlighted
    And the news feed is empty
    And the news feed is unavailable message is shown
