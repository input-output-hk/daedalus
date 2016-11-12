// @flow
import sinon from 'sinon';
import wallets from '../data/wallets.json';

export const loadWallets = sinon.stub();
loadWallets.yieldsAsync(null, wallets);
