<blockquote>
<sub>Document maintainer: Nikola Glumac<br/>Document status: Active</sub>
</blockquote>

# Running Daedalus acceptance tests


1. Make sure you have node and yarn installed on your machine
2. Clone Daedalus repository to your machine (`git clone git@github.com:input-output-hk/daedalus.git`)
3. Install dependencies from within Daedalus directory:

```bash
$ cd daedalus/
$ yarn install
```

4. Build and run the backend (Cardano SL) following the instructions from [Daedalus](https://github.com/input-output-hk/daedalus/blob/master/README.md#development---with-cardano-wallet) README file.
5. Run Daedalus frontend tests:

```bash
$ cd daedalus/
$ nix-shell --arg autoStartBackend true --arg systemStart XXX # XXX = cardano system startup time
$ yarn test
```

Once tests are complete you will get a summary of passed/failed tests in the Terminal window.

## Keeping Daedalus Alive After Tests

While working on the tests it's often useful to keep Daedalus alive after the tests have run 
(e.g: to inspect the app state). You can pass a special environment var to tell the test script
not to close the app:

````bash
$ KEEP_APP_AFTER_TESTS=true yarn test
````
