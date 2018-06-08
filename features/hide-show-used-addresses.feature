Feature: Hide/show used addresses

  Background:
    Given I have completed the basic setup
    And I have a "Genesis wallet" with funds
    And I have the following wallets:
      | name   |
      | TargetWallet |

  Scenario: After 1 transaction
    Given I am on the "TargetWallet" wallet "receive" screen
    And I generate 1 addresses
    And I have made the following transactions:
      | sender         | receiver      | amount |
      | Genesis wallet | TargetWallet  | 1      |
    Then I should see 2 addresses
    When I click the ShowUsed switch
    Then I should see 1 addresses
