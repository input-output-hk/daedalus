Feature: Hide/show used addresses

  The wallet receive screen has a switcher to hide used addresses

  Background:
    Given I have completed the basic setup
    And I have a "Genesis wallet" with funds
    And I am on the "Genesis wallet" wallet "receive" screen
    And I click on the "Generate new address" button

  Scenario: No click
    Given I have the following addresses
    | id                                                               | amount   | isUsed |
    | 6c179f21e6f62b629055d8ab40f454ed02e48b68563913473b857d3638e23b28 | 0.000123 | true   |
    | 399141be1d30ac2656d89eedcf0d8dcedaa72d6c29bf959cae243dc7b1442cf6 | 0.000234 | false  |
    | e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 | 0.000345 | false  |
    | 03e88438f3c62efefab36662464a3dcccbcdf8b7eca53a26090d101c3b5597e7 | 0.000345 | true   |
    Then I should see 2 addresses
