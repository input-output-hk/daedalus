@e2e @newsfeed @noReload
Feature: Newsfeed

  Newsfeed delivers important information to Daedalus users, like information about network failures, bugs and other
  issues, information about upcoming and recent releases and so on. News are categorised in 4 types:
  incident, alert, announcement and info. Incidents and alerts cover the whole user interface,
  announcements and info are delivered in the newsfeed part of the user interface.

  Background:
    Given I have completed the basic setup

  @reconnectApp @skip
  # @API TODO - We don't have network api endpoint
  Scenario: Newsfeed icon is visible on the connecting screen
    Given im on the connecting screen
    Then i should see the newsfeed icon

  @reconnectApp @skip
  # @API TODO - We don't have network api endpoint
  Scenario: Newsfeed icon is visible on the syncing screen
    Given im on the syncing screen
    Then i should see the newsfeed icon

  Scenario: Newsfeed icon is visible in the main ui
    Given I should see the main ui
    Then i should see the newsfeed icon

  Scenario: Newsfeed icon is highlighted when there are unread infos
    Given there are unread infos
    Then the newsfeed icon is highlighted

  Scenario: Newsfeed icon is highlighted when there are unread announcements
    Given there are unread announcements
    Then the newsfeed icon is highlighted

  Scenario: Newsfeed icon is not highlighted when all infos have been read
    Given there are read infos
    Then the newsfeed icon is not highlighted

  Scenario: Open the newsfeed by clicking the newsfeed icon
    Given I click on the newsfeed icon
    Then the newsfeed is open

  Scenario: No news available in the feed
    Given there is no news
    When I open the newsfeed
    Then the newsfeed is empty

  Scenario: Only read infos available in the feed
    Given there are 2 read infos
    When I open the newsfeed
    Then the newsfeed contains 2 read infos

  Scenario: Displaying an incident
    Given there is an incident
    Then the incident will cover the screen

  Scenario: Dismissing an alert
    Given there is 1 unread alert
    When I dismiss the alert
    Then the alert disappears

  Scenario: Opening and dismissing a read alert
    Given there is 1 read alert
    When I open the newsfeed
    When I click on the alert in the newsfeed
    Then the alert overlay opens
    When I dismiss the alert
    Then the alert disappears

  Scenario: Reading an announcement
    Given there is 1 unread announcement
    When I open the newsfeed
    And I click on the unread announcement to expand it
    Then the announcement content is shown
    And the announcement is marked as read

  Scenario: Reading an info
    Given there is 1 unread info
    When I open the newsfeed
    And I click on the unread info to expand it
    Then the info content is shown
    And the info is marked as read
    And the newsfeed icon is not highlighted

  Scenario: News unavailable
    Given the newsfeed server is unreachable
    When I open the newsfeed
    Then no news are available
