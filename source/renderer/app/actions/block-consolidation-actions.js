// @flow
import Action from './lib/Action';

// ======= NETWORK STATUS ACTIONS =======

export default class BlockConsolidationActions {
  startBlockConsolidationDataFetch: Action<any> = new Action();
  stopBlockConsolidationDataFetch: Action<any> = new Action();
}
