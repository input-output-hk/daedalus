// @flow
import Action from './lib/Action';

// ======= NETWORK STATUS ACTIONS =======

export default class BlockConsolidationActions {
  startBlockConsolidationDataPolling: Action<any> = new Action();
  stopBlockConsolidationDataPolling: Action<any> = new Action();
}
