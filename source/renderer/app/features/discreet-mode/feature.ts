import { observable, action, runInAction } from 'mobx';
import { Feature } from '../../utils/mobx-features/feature';
import Request from '../../stores/lib/LocalizedRequest';
import { DiscreetModeApi } from './api';
import { SENSITIVE_DATA_SYMBOL } from './config';
import { defaultReplacer } from './replacers/defaultReplacer';
import type { ReplacerFn } from './types';

export class DiscreetMode extends Feature {
  api: DiscreetModeApi;

  constructor(api: DiscreetModeApi) {
    super();
    this.api = api;
    runInAction(() => {
      this.getDiscreetModeSettingsRequest = new Request(
        this.api.getDiscreetModeSettings
      );
      this.setDiscreetModeSettingsRequest = new Request(
        this.api.setDiscreetModeSettings
      );
    });
  }

  @observable
  isDiscreetMode = false;
  @observable
  openInDiscreetMode = false;
  @observable
  getDiscreetModeSettingsRequest: Request<Promise<boolean>>;
  @observable
  setDiscreetModeSettingsRequest: Request<Promise<boolean>>;

  async start() {
    super.start();
    await this._setupDiscreetMode();
  }

  _setupDiscreetMode = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getDiscreetModeSettingsRequest.execute();
    const isDiscreetModeEnabled = this.getDiscreetModeSettingsRequest.result;
    runInAction('Initialize discreet mode variables', () => {
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Promise<boolean>' is not assignable to type ... Remove this comment to see the full error message
      this.openInDiscreetMode = isDiscreetModeEnabled;
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Promise<boolean>' is not assignable to type ... Remove this comment to see the full error message
      this.isDiscreetMode = isDiscreetModeEnabled;
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

  discreetValue({
    replacer = defaultReplacer(),
    value,
  }: {
    replacer?: ReplacerFn;
    value?: any;
  }) {
    return replacer(this.isDiscreetMode, SENSITIVE_DATA_SYMBOL, value);
  }
}
