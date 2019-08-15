<blockquote>
<sub>Document maintainer: Nikola Glumac, Marcus Hurney<br/>Document status: Active</sub>
</blockquote>

# Daedalus Acceptance Tests

### File types and Cucumber syntax

- Cucumber looks for files with the `.feature` file extension as a starting point for executing the `scenarios` of defined within the `.feature` file. Each `scenario` contains one or more `steps`. Together, the collective `scenarios`  within a `.feature` file comprise test coverage of at least one feature within the Daedalus UI.

### JavaScript Executables

- Each `step` in a `.feature` file will match a JavaScript `string` within a separate `.js` file containing the executable JavaScript that runs the test itself. The text of a `step` within a `.feature` file must precisely match its associated JavaScript `string` in order for the test to execute.

### File Structure

- All the files comprising the Daedalus acceptance tests are divided into directories by domain. A test belongs to a domain depending on the category of functionality it's meant to test. Within Daedalus the domains `Wallets`, `Paper Wallets`, `Addresses`, `Transactions`, `Staking`, `Status`, and `Settings`. These domains also constitute the top level directories of the Daedalus acceptance tests.
