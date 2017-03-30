// @flow
import { observable, action } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import Request from './lib/Request';
import globalMessages from '../i18n/global-messages';

export default class WalletSettingsStore extends Store {

  WALLET_ASSURANCE_LEVEL_OPTIONS = [
    { value: 'CWANormal', label: globalMessages.assuranceLevelNormal },
    { value: 'CWAStrict', label: globalMessages.assuranceLevelStrict },
  ];

  WALLET_UNIT_OPTIONS = [
    { value: 0, label: globalMessages.unitAda },
    { value: 1, label: globalMessages.unitLovelace },
  ];

  @observable updateWalletRequest = new Request(this.api, 'updateWallet');

  setup() {
    const a = this.actions.walletSettings;
    a.updateWalletAssuranceLevel.listen(this._updateWalletAssuranceLevel);
    a.updateWalletUnit.listen(this._updateWalletUnit);
  }

  @action _updateWalletAssuranceLevel = async ({ assurance }: { assurance: string }) => {
    const { id: walletId, type, currency, name, unit } = this.stores.wallets.active;
    await this.updateWalletRequest.execute({ walletId, type, currency, name, assurance, unit });
    await this.stores.wallets.walletsRequest.patch(result => {
      const wallet = _.find(result, { id: walletId });
      wallet.assurance = assurance;
    });
  };

  @action _updateWalletUnit = async ({ unit }: { unit: number }) => {
    const { id: walletId, type, currency, name, assurance } = this.stores.wallets.active;
    await this.updateWalletRequest.execute({ walletId, type, currency, name, assurance, unit });
    await this.stores.wallets.walletsRequest.patch(result => {
      const wallet = _.find(result, { id: walletId });
      wallet.unit = unit;
    });
  };

}
