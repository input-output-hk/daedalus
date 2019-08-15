<blockquote>
<sub>Document maintainer: Nikola Glumac, Marcus Hurney<br/>Document status: Active</sub>
</blockquote>

# Daedalus Acceptance Tests

### File types and Cucumber syntax

- Cucumber looks for files with a `.feature` file extension as the starting point for executing `scenarios` defined within a given `.feature` file. Each `scenario` contains one or more `steps`. `scenarios` describe the broader context or purpose of a given set of steps while the `steps` themselves describe the intended functionality of the Javascript test executables. Together, the collective `scenarios`  within a `.feature` file comprise test coverage of at least one feature within the Daedalus UI.

### JavaScript Executables

- Each `step` in a `.feature` file will match a JavaScript `string` passed as the first parameter to a `step-definition` function within a separate `.js` file. A `step-definition` also contains the executable JavaScript function(s) that run the test logic itself. The `step`'s name written as text within a `.feature` file must exactly match the associated JavaScript `string` within a `step-definition` in order for the executable to run.

### File Structure

- All the files comprising the Daedalus acceptance tests are divided into directories by domain. A test belongs to a domain depending on the category of functionality it's meant to test. Within Daedalus the domains `Wallets`, `Paper Wallets`, `Addresses`, `Transactions`, `Staking`, `Status`, and `Settings`. These domains also constitute the top level directories of the Daedalus acceptance tests.
