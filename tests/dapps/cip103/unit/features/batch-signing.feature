@unit @cip103
Feature: CIP-103 ordered batch signing

  Scenario: Software wallet releases one complete ordered witness batch
    Given a two-item CIP-103 batch for a "software" wallet
    When the user approves the CIP-103 batch
    Then the connector releases both verified witnesses in caller order

  Scenario Outline: Mocked hardware wallets release only complete ordered batches
    Given a two-item CIP-103 batch for a "<wallet>" wallet
    When the user approves the CIP-103 batch
    Then the connector releases both verified witnesses in caller order
    And the hardware batch requires no software passphrase

    Examples:
      | wallet |
      | Ledger |
      | Trezor |
