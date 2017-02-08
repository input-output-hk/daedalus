Feature: Switching Between Wallets

  Scenario Outline: Using the Sidebar to Switch Wallets
    Given I have the following wallets:
    | name   |
    | first  |
    | second |
    | third  |
    And I am on the <START> wallet
    And The sidebar shows the wallets category
    When I click on the <TARGET> wallet in the sidebar
    Then I should be on the <TARGET> wallet home screen

    Examples:
    | START  | TARGET |
    | first  | second |
    | second | third  |

