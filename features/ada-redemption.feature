@ui
Feature: Ada Redemption

  Background:
    Given I have completed the basic setup
    And I have the following wallets:
      | name        |
      | Test wallet |

  Scenario: User accepts "Daedalus Redemption Disclaimer"
    Given I am on the ada redemption screen
    And I see the "Daedalus Redemption Disclaimer" overlay
    And I click on the "I've understood the information above" checkbox
    When I click on the "Continue" button
    Then I should not see the "Daedalus Redemption Disclaimer" overlay anymore
    And I should still be on the ada redemption screen

  Scenario: User redeems manually entered "Regular" redemption key
    Given I am on the ada redemption screen
    And I have accepted "Daedalus Redemption Disclaimer"
    And I enter a valid "Regular" redemption key
    And ada redemption form submit button is no longer disabled
    When I submit the ada redemption form
    Then I should see the "Ada Redemption Success Overlay"

  Scenario: User redeems "Regular" PDF certificate
    Given I am on the ada redemption screen
    And I have accepted "Daedalus Redemption Disclaimer"
    And I select a valid "Regular" PDF certificate
    And ada redemption form submit button is no longer disabled
    When I submit the ada redemption form
    Then I should see the "Ada Redemption Success Overlay"

  Scenario: User redeems "Regular" encrypted PDF certificate
    Given I am on the ada redemption screen
    And I have accepted "Daedalus Redemption Disclaimer"
    And I select a valid "Regular" encrypted PDF certificate
    And I enter a valid "Regular" encrypted PDF certificate passphrase
    And ada redemption form submit button is no longer disabled
    When I submit the ada redemption form
    Then I should see the "Ada Redemption Success Overlay"

  Scenario: User redeems manually entered "Force vended" redemption key
    Given I am on the ada redemption screen
    And I have accepted "Daedalus Redemption Disclaimer"
    And I click on ada redemption choices "Force vended" tab
    And I enter a valid "Force vended" redemption key
    And ada redemption form submit button is no longer disabled
    When I submit the ada redemption form
    Then I should see the "Ada Redemption Success Overlay"

  Scenario: User redeems "Force vended" PDF certificate
    Given I am on the ada redemption screen
    And I have accepted "Daedalus Redemption Disclaimer"
    And I click on ada redemption choices "Force vended" tab
    And I select a valid "Force vended" PDF certificate
    And ada redemption form submit button is no longer disabled
    When I submit the ada redemption form
    Then I should see the "Ada Redemption Success Overlay"

  Scenario: User redeems "Force vended" encrypted PDF certificate
    Given I am on the ada redemption screen
    And I have accepted "Daedalus Redemption Disclaimer"
    And I click on ada redemption choices "Force vended" tab
    And I select a valid "Force vended" encrypted PDF certificate
    And I enter a valid "Force vended" encrypted PDF certificate email, passcode and amount
    And ada redemption form submit button is no longer disabled
    When I submit the ada redemption form
    Then I should see the "Ada Redemption Success Overlay"

  Scenario: User redeems manually entered "Paper vended" shielded vending key and passphrase
    Given I am on the ada redemption screen
    And I have accepted "Daedalus Redemption Disclaimer"
    And I click on ada redemption choices "Paper vended" tab
    And I enter a valid "Paper vended" shielded vending key
    And I enter a valid "Paper vended" shielded vending key passphrase
    And ada redemption form submit button is no longer disabled
    When I submit the ada redemption form
    Then I should see the "Ada Redemption Success Overlay"

  Scenario: User redeems "Recovery - regular" encrypted PDF certificate
    Given I am on the ada redemption screen
    And I have accepted "Daedalus Redemption Disclaimer"
    And I click on ada redemption choices "Recovery - regular" tab
    And I select a valid "Regular" encrypted PDF certificate
    And I enter a valid "Regular" encrypted PDF certificate passphrase
    And ada redemption form submit button is no longer disabled
    When I submit the ada redemption form
    Then I should see the "Ada Redemption Success Overlay"

  Scenario Outline: User redeems "Recovery - force vended" encrypted PDF certificate
    Given I am on the ada redemption screen
    And I have accepted "Daedalus Redemption Disclaimer"
    And I click on ada redemption choices "Recovery - force vended" tab
    And I select a valid "Force vended" encrypted PDF certificate
    And I enter a valid "Force vended" encrypted PDF certificate decryption key "<KEY>"
    And ada redemption form submit button is no longer disabled
    When I submit the ada redemption form
    Then I should see the "Ada Redemption Success Overlay"

    Examples:
    | KEY                                                                                                                                                |
    | qXQWDxI3JrlFRtC4SeQjeGzLbVXWBomYPbNO1Vfm1T4=                                                                                                       |
    | A974160F123726B94546D0B849E423786CCB6D55D60689983DB34ED557E6D53E                                                                                   |
    | [ 169, 116, 22, 15, 18, 55, 38, 185, 69, 70, 208, 184, 73, 228, 35, 120, 108, 203, 109, 85, 214, 6, 137, 152, 61, 179, 78, 213, 87, 230, 213, 62 ] |
