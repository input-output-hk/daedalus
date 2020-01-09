<blockquote>
<sub>Document maintainer: Nikola Glumac, Marcus Hurney<br/>Document status: Active</sub>
</blockquote>

# Daedalus Acceptance Tests Overview

### File types and Cucumber syntax

- Cucumber looks for files with a `.feature` file extension as the starting point for executing `scenarios` defined within a given `.feature` file. Each `scenario` contains one or more `steps`. `scenarios` describe the broader context or purpose of a given set of steps while the `steps` themselves describe the intended functionality of the Javascript test executables. Together, the collective `scenarios`  within a `.feature` file comprise test coverage of at least one feature within the Daedalus UI.

### JavaScript Executables

- Each `step` in a `.feature` file will match a JavaScript `string` passed as the first parameter to a `step-definition` function within a separate `.js` file. A `step-definition` also contains the executable JavaScript function(s) that run the test logic itself. The `step`'s name written as text within a `.feature` file must exactly match the associated JavaScript `string` within a `step-definition` in order for the executable to run.

### File Structure

- All the files comprising the Daedalus acceptance tests are divided into directories by domain. A test belongs to a domain depending on the category of functionality it's meant to test. Within Daedalus the domains `wallets`, `paper-wallets`, `addresses`, `transactions`, `navigation`, `nodes`, `settings`, and `common`. These domains also constitute the top level directories of the Daedalus acceptance tests.

# Running Daedalus Acceptance Tests

### Install Daedalus

1. Make sure you have node and yarn installed on your machine
2. Clone Daedalus repository to your machine (`git clone git@github.com:input-output-hk/daedalus.git`)
3. Install dependencies from within Daedalus directory:

```bash
$ yarn install
```

### Run unit tests

Make sure Daedalus is properly installed (see above).

```bash
$ yarn test:unit
```

### Unbound tests
   
Unbound tests run as long as you keep them running 
(never end except if an error occurs).
   
Example:
`yarn test:unit:unbound --tags @mnemonics` 
generates and validates mnemonics as long as you keep it 
running (the number of executions is updated in the terminal)

### Run end-to-end tests with Jörmungandr self-node

1. Make sure Daedalus is properly installed (see above).
2. Make sure your state directory is clean (`rm -rf ~/Library/Application\ Support/Daedalus\ SelfNode/`)
3. Run Daedalus frontend tests:

```bash
$ cd daedalus/
$ yarn nix:dev
$ yarn build
$ yarn test:e2e
```

### Running tests for development
1. Mark the test or scenario you are working with @watch annotation
2. Make sure you are in the nix console (`yarn nix:dev`)
3. Make sure your state is clean (`rm -rf ~/Library/Application\ Support/Daedalus\ SelfNode/`)
4. Run tests with `yarn test:e2e:watch:once`

### Run all tests

```bash
$ yarn test
```

Once tests are complete you will get a summary of passed/failed tests in the Terminal window.

### Keeping Daedalus alive after end-to-end tests

While working on the tests it's often useful to keep Daedalus alive after the tests have run 
(e.g: to inspect the app state). You can pass a special environment var to tell the test script
not to close the app:

````bash
$ KEEP_APP_AFTER_TESTS=true yarn test:e2e
````
