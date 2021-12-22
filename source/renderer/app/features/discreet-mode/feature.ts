import { observable, action, runInAction } from 'mobx';
import { Feature } from '../../utils/mobx-features/feature';
import Request from '../../stores/lib/LocalizedRequest';
import { DiscreetModeApi } from './api';
import { SENSITIVE_DATA_SYMBOL } from './config';
import { defaultReplacer } from './replacers/defaultReplacer';
import type { ReplacerFn } from './types';

export class DiscreetMode extends Feature {
  api: DiscreetModeApi;
  @observable
  isDiscreetMode = false;
  @observable
  openInDiscreetMode = false;
  @observable
  isSettingsTooltipEnabled = false;
  @observable
  isNotificationEnabled = false;
  @observable
  getDiscreetModeSettingsRequest: Request<Promise<boolean>> = new Request(
    // @ts-ignore ts-migrate(2729) FIXME: Property 'api' is used before its initialization.
    this.api.getDiscreetModeSettings
  );
  @observable
  setDiscreetModeSettingsRequest: Request<Promise<boolean>> = new Request(
    // @ts-ignore ts-migrate(2729) FIXME: Property 'api' is used before its initialization.
    this.api.setDiscreetModeSettings
  );
  @observable
  getDiscreetModeSettingsTooltipRequest: Request<
    Promise<boolean>
    // @ts-ignore ts-migrate(2729) FIXME: Property 'api' is used before its initialization.
  > = new Request(this.api.getDiscreetModeSettingsTooltip);
  @observable
  setDiscreetModeSettingsTooltipRequest: Request<
    Promise<boolean>
    // @ts-ignore ts-migrate(2729) FIXME: Property 'api' is used before its initialization.
  > = new Request(this.api.setDiscreetModeSettingsTooltip);
  @observable
  getDiscreetModeNotificationRequest: Request<Promise<boolean>> = new Request(
    // @ts-ignore ts-migrate(2729) FIXME: Property 'api' is used before its initialization.
    this.api.getDiscreetModeNotification
  );
  @observable
  setDiscreetModeNotificationRequest: Request<Promise<boolean>> = new Request(
    // @ts-ignore ts-migrate(2729) FIXME: Property 'api' is used before its initialization.
    this.api.setDiscreetModeNotification
  );

  constructor(api: DiscreetModeApi) {
    super();
    this.api = api;
  }

  async start() {
    super.start();
    await this._setupDiscreetMode();
  }

  _setupDiscreetMode = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getDiscreetModeSettingsRequest.execute();
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getDiscreetModeSettingsTooltipRequest.execute();
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getDiscreetModeNotificationRequest.execute();
    const isDiscreetModeEnabled = this.getDiscreetModeSettingsRequest.result;
    const isSettingsTooltipEnabled = this.getDiscreetModeSettingsTooltipRequest
      .result;
    const isNotificationEnabled = this.getDiscreetModeNotificationRequest
      .result;
    runInAction('Initialize discreet mode variables', () => {
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Promise<boolean>' is not assignable to type ... Remove this comment to see the full error message
      this.openInDiscreetMode = isDiscreetModeEnabled;
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Promise<boolean>' is not assignable to type ... Remove this comment to see the full error message
      this.isDiscreetMode = isDiscreetModeEnabled;
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Promise<boolean>' is not assignable to type ... Remove this comment to see the full error message
      this.isSettingsTooltipEnabled = isSettingsTooltipEnabled;
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Promise<boolean>' is not assignable to type ... Remove this comment to see the full error message
      this.isNotificationEnabled = isNotificationEnabled;
    });
  };
  @action
  toggleDiscreetMode = () => {
    this.isDiscreetMode = !this.isDiscreetMode;
  };
  @action
  toggleOpenInDiscreetMode = async () => {
    const nextSetting = !this.openInDiscreetMode;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setDiscreetModeSettingsRequest.execute(nextSetting);
    runInAction('Update open in discreet mode settings', () => {
      this.openInDiscreetMode = nextSetting;
    });
  };
  @action
  setDiscreetModeSettingsTooltip = async (enabled: boolean) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setDiscreetModeSettingsTooltipRequest.execute(enabled);
    runInAction('Update discreet mode settings tooltip', () => {
      this.isSettingsTooltipEnabled = enabled;
    });
  };
  @action
  setDiscreetModeNotification = async (enabled: boolean) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setDiscreetModeNotificationRequest.execute(enabled);
    runInAction('Update discreet mode notification', () => {
      this.isNotificationEnabled = enabled;
    });
  };

  discreetValue({
    replacer = defaultReplacer(),
    value,
  }: {
    replacer?: ReplacerFn;
    value?: number | string;
  }) {
    return replacer(this.isDiscreetMode, SENSITIVE_DATA_SYMBOL, value);
  }
}
