# Update system notes

These are notes on how the Cardano wallet launcher and update system
currently work.

## Old Design

### Diagram

This is the `frontendOnlyScenario` from `cardano-sl/tools/src/launcher/Main.hs`.
Do not worry about `serverScenario` - replace it completely.

The Daedalus install is always configured to run the launcher in
`frontendOnlyMode`. The other code (`clientScenario`) is dead
code. The diagram describes `frontendOnlyMode`, which goes through the
`node-ipc` system.

![sequence-chart](update-system-old.png)

#### Running the update system

The very first thing that `cardano-launcher` does, before starting the
frontend, is check for a previously downloaded update.

If there is a file at `updaterPath`, it runs the updater, which is a
bit wrong.

There are two code paths, depending on whether launcher is running on Windows.

##### macOS and Linux

To run the updater on non-Windows systems, it will spawn
`${updaterPath} ${updaterArgs} ${updateArchive}`.

After the update subprocess exits successfully, it hashes the
`updateArchive` file, checks that the wallet database has this hash,
then deletes the `updateArchive` file.

##### Windows

To run the updater on Windows systems, it will create a `.bat` file
then execute it. The temporary `.bat` file runs the following steps:

1. Kill the `cardano-launcher` process with `TaskKill /PID`
2. Run the downloaded installer for the new version.
3. Delete the installer file.
4. Run `cardano-launcher` with the same command line arguments that
   the previous launcher was run with.
5. Delete the `.bat` file.

#### Starting Daedalus

Daedalus is started by running Electron, with the main Javascript file
being in a subdirectory (the exact filename is configured by the
`main` key of `package.json`)

The Daedalus Window title bar is configured using the `productName`
key of `package.json`.

Daedalus uses a number of variables at runtime.

| Variable          | Description                                             | Set by   |
| ----------------- |:------------------------------------------------------- | -------- |
| `NETWORK`         | Used to show the user what network Daedalus is          | Build    |
|                   | connecting to. One of `mainnet`, `staging`, `testnet`.  |          |
| `REPORT_URL`      | The base URL for posting Daedalus automatic bug         | Build    |
|                   | reports.                                                |          |
| `API_VERSION`     | Used to show the Cardano version in the About box.      | Build    |
| `LAUNCHER_CONFIG` | Path to a launcher config YAML file.                    | Launcher |

The variables listed as being set by "Build" are in fact hard-coded
using string substitution at installer build-time, but this need not
be the case.

#### Starting `cardano-node`

Daedalus spawns the `cardano-node` process using
[child_process](https://nodejs.org/api/child_process.html). The
command-line parameters are built from the file specified by
`LAUNCHER_CONFIG`.

After starting the `cardano-node` process, it then uses the nodejs API
to pass "messages" to the subprocess (see
`cardano-sl/node-ipc/src/Cardano/NodeIPC.hs`).


#### Applying updates

To apply an update, `cardano-launcher` just loops back to the start,
where the update system will immediately run.

#### Update server

The update server is a S3 bucket managed by the IOHK Service Reliability team, accessible by
HTTPS. Before proposing an update on the blockchain, the installer
files are uploaded to the updates bucket, named with their Blake2b
hash.

As an example: [the testnet update server](https://updates-cardano-testnet.s3.amazonaws.com/index.html)
