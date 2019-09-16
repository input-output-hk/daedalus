@wip
Feature: News feed

  News feed delivers

  Scenario: No news available



    Given I have Daedalus running
    And there is no news
    Then The bell icon is not highlited

  Scenario: Only read news available

    Given I have Daedalus running
    And news feed is not empty
    And all news items have been read
    Then The bell icon is not highlited
    And


  Scenario: Incident

  Scenario: Alert

  Scenario: Announcement

  Scenario: Info

  Scenario: News unavailable



