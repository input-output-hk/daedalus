Feature: Delete Wallet via Settings

  Background:
    Given I have selected English language
    And I have accepted "Terms of use"
    And I have a wallet with funds

  Scenario: Successfully Deleting a Wallet
    Given I am on the "Genesis wallet" wallet "settings" screen
    When I click on delete wallet button
    And I see delete wallet dialog
    And I click on the "Make sure you have access to backup before continuing" checkbox
    And I enter "Genesis wallet" as name of the wallet to confirm
    And I submit the delete wallet dialog
    Then I should not see the delete wallet dialog anymore
