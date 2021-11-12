// @flow

import { observable, action, runInAction } from 'mobx';
import { Feature } from '../../utils/mobx-features/feature';
import Request from '../../stores/lib/LocalizedRequest';
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

  hideSensitiveData(data: string) {
    if (!this.isDiscreetMode) {
      return data;
    }

    const content = data.split(' ');

    if (content.length <= 1) {
      return SENSITIVE_DATA_SYMBOL;
    }

    const tickerSymbol = content.pop();

    return `${SENSITIVE_DATA_SYMBOL} ${tickerSymbol}`;
  }
}
