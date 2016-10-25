Feature: Send Money to Receiver

  Scenario: User Submits Empty Form
    Given I am on the wallet send screen
    When I submit the wallet send form
    Then I should see the following error messages:
    # TODO: use translation IDs for these examples instead
    | message                               |
    | Please enter a valid Bitcoin address. |
    | Please enter a valid amount.          |
