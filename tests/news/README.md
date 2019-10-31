<blockquote>
<sub>Document maintainer: Tomislav Horaček<br/>Document status: Active</sub>
</blockquote>

# Fetch newsfeed from local JSON files

- Newsfeed files are always fetched from the server and verified with hash.
- For development purposes, helper commands are enabled. By entering commands newsfeed JSON files are loaded from local folder `source/renderer/app/config/newsfeed-files/`. 
- Commands that can fetch newsfeed from local JSON files are available only in a `development` environment and must be called as console commands in app run-time.

Available files are:
1. newsfeed_development.json
2. newsfeed_mainnet.json
3. newsfeed_staging.json
4. newsfeed_testnet.json

### Commands

```bash
daedalus.stores.newsFeed.getNewsFromLocalFiles(isLocal, environment)
```

### Example: Fetch newsfeed from `newsfeed_development.json` file

```bash
daedalus.stores.newsFeed.getNewsFromLocalFiles(true, 'development')
```

- Once a command is entered, all pollers are stopped and fetching from the server is disabled. To re-enable server fetching, enter command:

```bash
daedalus.stores.newsFeed.getNewsFromLocalFiles(false)
```