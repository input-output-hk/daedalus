@e2e
Feature: Internet Connection Status

  Background:
    Given I have app setup completely

  Scenario: Internet Connection Online
    Given App runs while physical internet connection is online
    When I watch the app screen
    Then Internet connection offline overlay should never open automatically

  Scenario: Internet Connection Offline
    Given App runs while physical internet connection is offline
    When I watch the app screen
    Then Internet connection offline overlay should open automatically
