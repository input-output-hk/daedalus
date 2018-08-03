<blockquote>
<sub>Document maintainer: Nikola Glumac<br/>Document status: Active</sub>
</blockquote>

# Running Daedalus acceptance tests


1. Make sure you have the correct node/npm versions installed on your machine (node v8.x and npm v5.x)
2. Clone Daedalus repository to your machine (`git clone git@github.com:input-output-hk/daedalus.git` - use **master** branch)
3. Install npm dependencies from within Daedalus directory:

```bash
$ cd daedalus/
$ yarn install
```
4. Build and run the backend (Cardano SL) following the instructions from [Daedalus](https://github.com/input-output-hk/daedalus/blob/master/README.md#development---with-cardano-wallet) README file.
5. Run Daedalus frontend tests:

```bash
$ cd daedalus/
$ yarn run test
```

Once tests are complete you will get a summary of passed/failed tests in the Terminal window.
