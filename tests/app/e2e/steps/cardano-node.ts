import { Given, Then } from "cucumber";
import { CardanoNodeStates } from "../../../../source/common/types/cardano-node.types";
import { getCardanoNodeState, waitForCardanoNodeToExit } from "./helpers";

Given(/^cardano-node is running$/, async function () {
  await this.client.waitUntil(async () => (await getCardanoNodeState(this.client)) === CardanoNodeStates.RUNNING);
});
Then(/^cardano-node process is not running$/, {
  timeout: 61000
}, async function () {
  await waitForCardanoNodeToExit(this.client);
});