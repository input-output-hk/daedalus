import { observable, action, runInAction, makeObservable } from 'mobx';
import { Feature } from '../../utils/mobx-features/feature';
import Request from '../../stores/lib/LocalizedRequest';
import { DiscreetModeApi } from './api';
import { SENSITIVE_DATA_SYMBOL } from './config';
import { defaultReplacer } from './replacers/defaultReplacer';
import type { ReplacerFn } from './types';
import { AnalyticsTracker, EventCategories } from '../../analytics';

export class DiscreetMode extends Feature {
  constructor(
    private api: DiscreetModeApi,
    private analyticsTracker: AnalyticsTracker
  ) {
    super();

    makeObservable(this, {
      isDiscreetMode: observable,
      openInDiscreetMode: observable,
      getDiscreetModeSettingsRequest: observable,
      setDiscreetModeSettingsRequest: observable,
      toggleDiscreetMode: action,
      toggleOpenInDiscreetMode: action,
    });

    runInAction(() => {
      this.getDiscreetModeSettingsRequest = new Request(
        this.api.getDiscreetModeSettings
      );
      this.setDiscreetModeSettingsRequest = new Request(
        this.api.setDiscreetModeSettings
      );
    });
  }

  isDiscreetMode = false;
  openInDiscreetMode = false;
  getDiscreetModeSettingsRequest: Request<Promise<boolean>>;
  setDiscreetModeSettingsRequest: Request<Promise<boolean>>;

  async start() {
    super.start();
    await this._setupDiscreetMode();
  }

  _setupDiscreetMode = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getDiscreetModeSettingsRequest.execute();
    const isDiscreetModeEnabled = this.getDiscreetModeSettingsRequest.result;
    runInAction(() => {
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Promise<boolean>' is not assignable to type ... Remove this comment to see the full error message
      this.openInDiscreetMode = isDiscreetModeEnabled;
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Promise<boolean>' is not assignable to type ... Remove this comment to see the full error message
      this.isDiscreetMode = isDiscreetModeEnabled;
    });
  };
  toggleDiscreetMode = () => {
    this.isDiscreetMode = !this.isDiscreetMode;
    this.analyticsTracker.sendEvent(
      EventCategories.SETTINGS,
      `Turned ${this.isDiscreetMode ? 'on' : 'off'} discreet mode`
    );
  };
  toggleOpenInDiscreetMode = async () => {
    const nextSetting = !this.openInDiscreetMode;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setDiscreetModeSettingsRequest.execute(nextSetting);
    runInAction(() => {
      this.openInDiscreetMode = nextSetting;
    });
    this.analyticsTracker.sendEvent(
      EventCategories.SETTINGS,
      `Turned ${nextSetting ? 'on' : 'off'} discreet mode by default`
    );
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
