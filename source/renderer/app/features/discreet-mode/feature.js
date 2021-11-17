// @flow
import BigNumber from 'bignumber.js';
import { observable, action, runInAction } from 'mobx';
import { Feature } from '../../utils/mobx-features/feature';
import Request from '../../stores/lib/LocalizedRequest';
import type { AssetMetadata } from '../../api/assets/types';
import {
  formattedWalletAmount,
  formattedTokenWalletAmount,
} from '../../utils/formatters';
import { DiscreetModeApi } from './api';
import { SENSITIVE_DATA_SYMBOL } from './config';

export class DiscreetMode extends Feature {
  api: DiscreetModeApi;
  @observable isDiscreetMode: boolean = false;
  @observable openInDiscreetMode: boolean = false;

  @observable getDiscreetModeSettingsRequest: Request<
    Promise<boolean>
  > = new Request(this.api.getDiscreetModeSettings);

  @observable setDiscreetModeSettingsRequest: Request<
    Promise<boolean>
  > = new Request(this.api.setDiscreetModeSettings);

  constructor(api: DiscreetModeApi) {
    super();

    this.api = api;
  }

  async start() {
    super.start();

    await this._setupDiscreetMode();
  }

  _setupDiscreetMode = async () => {
    await this.getDiscreetModeSettingsRequest.execute();
    const isDiscreetModeEnabled = this.getDiscreetModeSettingsRequest.result;
    runInAction('Initialize discreet mode variables', () => {
      this.openInDiscreetMode = isDiscreetModeEnabled;
      this.isDiscreetMode = isDiscreetModeEnabled;
    });
  };

  @action toggleDiscreetMode = () => {
    this.isDiscreetMode = !this.isDiscreetMode;
  };

  @action toggleOpenInDiscreetMode = async () => {
    const nextSetting = !this.openInDiscreetMode;
    await this.setDiscreetModeSettingsRequest.execute(nextSetting);
    runInAction('Update open in discreet mode settings', () => {
      this.openInDiscreetMode = nextSetting;
    });
  };

  hideOrShowTokenWalletAmount({
    amount,
    metadata,
    decimals,
    isShort,
  }: {
    amount: BigNumber,
    metadata?: ?AssetMetadata,
    decimals: ?number,
    isShort?: boolean,
  }) {
    if (!this.isDiscreetMode) {
      return formattedTokenWalletAmount(amount, metadata, decimals, isShort);
    }

    const { ticker } = metadata || {};

    if (!ticker) {
      return SENSITIVE_DATA_SYMBOL;
    }

    return `${SENSITIVE_DATA_SYMBOL} ${ticker}`;
  }

  hideOrShowWalletAmount({
    amount,
    withCurrency = true,
    long = true,
  }: {
    amount: BigNumber,
    withCurrency?: boolean,
    long?: boolean,
  }) {
    if (!this.isDiscreetMode) {
      return formattedWalletAmount(amount, withCurrency, long);
    }

    if (!withCurrency) {
      return SENSITIVE_DATA_SYMBOL;
    }

    return `${SENSITIVE_DATA_SYMBOL} ADA`;
  }

  hideOrShowSensitiveData(data: string | number) {
    if (!this.isDiscreetMode) {
      return data;
    }

    return SENSITIVE_DATA_SYMBOL;
  }
}
