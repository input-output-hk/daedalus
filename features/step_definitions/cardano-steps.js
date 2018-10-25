// @flow
import { Given, Then } from 'cucumber';
import { CardanoNodeStates } from '../../source/common/types/cardanoNode.types';
import { getCardanoNodeState } from '../support/helpers/cardano-node-helpers';
import { getProcessesByName } from '../../source/main/utils/processes';

Given(/^cardano-node is running$/, async function () {
  return await this.client.waitUntil(async () => (
    await getCardanoNodeState(this.client) === CardanoNodeStates.RUNNING
  ));
});

Then(/^cardano-node process is not running$/, { timeout: 31000 }, async function () {
  return await this.client.waitUntil(async () => (
    (await getProcessesByName('cardano-node')).length === 0
  ), 31000);
});
