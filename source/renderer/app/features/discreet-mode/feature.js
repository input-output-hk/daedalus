// @flow
import { observable, action, runInAction } from 'mobx';
import { Feature } from '../../utils/mobx-features/feature';
import Request from '../../stores/lib/LocalizedRequest';

import { DiscreetModeApi } from './api';
import { SENSITIVE_DATA_SYMBOL } from './config';
import { defaultReplacer } from './replacers/defaultReplacer';
import type { ReplacerFn } from './types';

export class DiscreetMode extends Feature {
  api: DiscreetModeApi;
  @observable isDiscreetMode: boolean = false;
  @observable openInDiscreetMode: boolean = false;
  @observable isSettingsTooltipEnabled: boolean = false;
  @observable isNotificationEnabled: boolean = false;

  @observable getDiscreetModeSettingsRequest: Request<
    Promise<boolean>
  > = new Request(this.api.getDiscreetModeSettings);

  @observable setDiscreetModeSettingsRequest: Request<
    Promise<boolean>
  > = new Request(this.api.setDiscreetModeSettings);

  @observable getDiscreetModeSettingsTooltipRequest: Request<
    Promise<boolean>
  > = new Request(this.api.getDiscreetModeSettingsTooltip);

  @observable setDiscreetModeSettingsTooltipRequest: Request<
    Promise<boolean>
  > = new Request(this.api.setDiscreetModeSettingsTooltip);

  @observable getDiscreetModeNotificationRequest: Request<
    Promise<boolean>
  > = new Request(this.api.getDiscreetModeNotification);

  @observable setDiscreetModeNotificationRequest: Request<
    Promise<boolean>
  > = new Request(this.api.setDiscreetModeNotification);

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
    await this.getDiscreetModeSettingsTooltipRequest.execute();
    await this.getDiscreetModeNotificationRequest.execute();
    const isDiscreetModeEnabled = this.getDiscreetModeSettingsRequest.result;
    const isSettingsTooltipEnabled = this.getDiscreetModeSettingsTooltipRequest
      .result;
    const isNotificationEnabled = this.getDiscreetModeNotificationRequest
      .result;
    runInAction('Initialize discreet mode variables', () => {
      this.openInDiscreetMode = isDiscreetModeEnabled;
      this.isDiscreetMode = isDiscreetModeEnabled;
      this.isSettingsTooltipEnabled = isSettingsTooltipEnabled;
      this.isNotificationEnabled = isNotificationEnabled;
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

  @action setDiscreetModeSettingsTooltip = async (enabled: boolean) => {
    await this.setDiscreetModeSettingsTooltipRequest.execute(enabled);
    runInAction('Update discreet mode settings tooltip', () => {
      this.isSettingsTooltipEnabled = enabled;
    });
  };

  @action setDiscreetModeNotification = async (enabled: boolean) => {
    await this.setDiscreetModeNotificationRequest.execute(enabled);
    runInAction('Update discreet mode notification', () => {
      this.isNotificationEnabled = enabled;
    });
  };

  discreetValue({
    replacer = defaultReplacer(),
    value,
  }: {
    replacer?: ReplacerFn,
    value?: number | string,
  }) {
    return replacer(this.isDiscreetMode, SENSITIVE_DATA_SYMBOL, value);
  }
}
