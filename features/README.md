# Running Deadalus acceptance tests


1. Make sure you have correct node/npm versions installed on your machine (node v6.x and npm v3.x)
2. Clone Deadalus repo to your machine (git@github.com:input-output-hk/daedalus.git - use **master** branch)
3. Install npm dependencies from within Daedalus directory:
```
$ cd daedalus/
$ npm install
```
4. Link your backend build with the Daedalus frontend (assumed that you have build the backend previously):
```
$ cd daedalus/node_modules/
$ npm link daedalus-client-api
```
5. Launch the backend (cardano-sl) in production/staging mode:
```
$ cd cardano-sl/
$ ./scripts/launch/staging.sh
```
6. Run Deadalus frontend in hot-server mode:
```
$ cd daedalus/
$ npm run hot-server
```
7. Run Daedalus frontend tests in a separate Terminal window (leaving hot-server to run in the other):
```
$ cd daedalus/
$ npm run test
```

There is a total of 46 different acceptance tests.
Whole test suite takes around 5 minutes to finish.
Once tests are complete you will get a summary of passed/failed tests in the Terminal window.
Daedalus UI window will remain open - you are expected to close it manually before running tests again.
