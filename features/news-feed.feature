@e2e
Feature: Newsfeed

  Newsfeed delivers important information to Daedalus users, like information about network failures, bugs and other
  issues, information about upcoming and recent releases and so on. News are categorised in 4 types:
  incident, alert, announcement and info. Incidents and alerts cover the whole user interface,
  announcements and info are delivered in the newsfeed part of the user interface.

  Background:
    Given I have completed the basic setup

  @reconnectApp
  Scenario: Newsfeed icon is visible on the connecting screen
    Given im on the connecting screen
    Then i should see the newsfeed icon

  @reconnectApp
  Scenario: Newsfeed icon is visible on the syncing screen
    Given im on the syncing screen
    Then i should see the newsfeed icon

  Scenario: Newsfeed icon is visible in the main ui
    Given I should see the main ui
    Then i should see the newsfeed icon

  Scenario: Newsfeed icon is highlighted when there are unread news
    Given there are unread news
    Then the newsfeed icon is highlighted

  Scenario: Newsfeed icon is not highlighted when there are no unread news
    Given there are no unread news
    Then the newsfeed icon is not highlighted

  Scenario: Newsfeed icon shows a dot when there are unread announcements
    Given there is unread announcements
    Then the newsfeed icon shows a dot

  Scenario: Open the newsfeed by clicking the newsfeed icon
    Given I click on the newsfeed icon
    Then the newsfeed is open

  Scenario: No news available in the feed
    Given there is no news
    When I open the newsfeed
    Then the newsfeed is empty

  @wip
  Scenario: Only read news available in the feed
    Given there are 5 read news
    When I open the newsfeed
    Then the newsfeed contains 5 read news

  @watch
  Scenario: Displaying an incident
    Given there is an incident
    Then the incident will cover the screen

  @wip
  Scenario: Dismissing an alert
    Given there are unread alerts
    And the latest alert will cover the screen
    When I dismiss the alert
    Then the alert I have dismissed becomes read

  @wip
  Scenario: Reading an announcement
    Given there is 1 unread announcement
    When I click on the newsfeed icon
    Then the newsfeed is opened
    And the newsfeed contains 1 unread announcement
    When I click on the unread announcement to expand it
    Then the announcement content is shown
    And the announcement is marked as read

  @wip
  Scenario: Reading an info
    Given there is 1 unread info
    When I click on the newsfeed icon
    Then the newsfeed is opened
    And the newsfeed contains 1 unread info
    When I click on the unread info to expand it
    Then the info content is shown
    And the info is marked as read
    And the newsfeed icon is not highlighted

  @wip
  Scenario: News unavailable
    Given the newsfeed server is unreachable
    When I open the newsfeed
    Then no news are available
