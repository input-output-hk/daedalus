@e2e @skip
Feature: App Update

  Scenario: 2.2.0 not installed, download not started
    Given I open the 2.1.0 build
    And the download was not started
    Then I should see the green dot in the notification bell
    When I click the bell
    Then I should see the download progress as a first Newsfeed item
    When I click the Newsfeed item
    Then I should see the Update Overlay
    And the overlay should display the download progress
    And the overlay should display a close button
    And the overlay should close when clicking on any area

  Scenario: 2.2.0 not installed, download was interrupted
    Given I open the 2.1.0 build
    And the download was not started
    Then I should see the green dot in the notification bell
    When I click the bell
    Then I should see the download progress as a first Newsfeed item
    When I close and reopen the app
    Then I should see the green dot in the notification bell
    When I click the bell
    Then I should see the download progress as a first Newsfeed item
    And the download should resume from the same percentage as it was before closing

  Scenario: 2.2.0 not installed, download is completed
    Given I open the 2.1.0 build
    And the download was finished with success
    Then I should see the update overlay
    And the overlay should not have a progress bar
    And the overlay should have a checkbox and a button
    When I click the checkbox and the button
    Then the app should close
    And the installer should open

  Scenario: 2.2.0 not installed, download is completed, app reopened
    Given I open the 2.1.0 build
    And the download was finished with success
    Then I should see the update overlay
    When I close and reopen the app
    Then I should see the update overlay
    And the overlay should have a link underneath the main button, for a manual installation

  Scenario: 2.2.0 not installed, download is interrupted, file is deleted
    Given I open the 2.1.0 build
    And the download was not started
    Then I should see the green dot in the notification bell
    When I close the app
    And delete the temporary file
    And reopen the app
    Then I should see the update overlay
    And the overlay should display a red text saying "We were unable to launch the update installer automatically. Please manually update Daedalus to its latest version."
    And the have a button for the manual update
    When I close and reopen the app
    Then I should still see the overlay with manual update info

  Scenario: 2.2.0 not installed, download is completed, file is deleted
    Given I open the 2.1.0 build
    And the download was finished with success
    When I close the app
    And delete the temporary file
    And reopen the app
    Then I should see the update overlay
    When I click the checkbox and the button
    Then I should see the overlay with manual update info

  Scenario: 2.2.0 is installed
    Given I open the 2.0.0 build
    Then I should not see the green dot in the bell
    And I should not see the update item in the newsfeed
    And I should not see the update overlay










