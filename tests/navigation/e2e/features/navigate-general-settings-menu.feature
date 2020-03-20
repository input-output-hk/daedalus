@e2e @byron
Feature: General Settings

  Background:
    Given I have completed the basic setup
    And I have the following "Rewards" wallets:
      | name        |
      | Test Wallet |

  Scenario Outline: Navigating through General Settings secondary menu
    Given I am on the General Settings "<FROM>" screen
    When I click on secondary menu "<TO>" item
    Then I should see General Settings "<TO>" screen

    Examples:
    | FROM             | TO               |
    | general          | display          |
    | general          | terms-of-service |
    | general          | support          |
    | display          | general          |
    | display          | terms-of-service |
    | display          | support          |
    | terms-of-service | general          |
    | terms-of-service | display          |
    | terms-of-service | support          |
    | support          | general          |
    | support          | display          |
    | support          | terms-of-service |

  Scenario: Change language in General Settings
    Given I am on the General Settings "general" screen
    And I open General Settings language selection dropdown
    And I select Japanese language
    Then I should see Japanese language as selected

  Scenario: Change theme in General Settings
    Given I am on the General Settings "display" screen
    And I select second theme
    Then I should see second theme as selected

  Scenario: Change send-logs option in General Settings
    Given I am on the General Settings "support" screen
    Then I should see the page with Frequency asked questions title
