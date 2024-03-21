'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.DiscreetMode = void 0;
const mobx_1 = require('mobx');
const feature_1 = require('../../utils/mobx-features/feature');
const LocalizedRequest_1 = __importDefault(
  require('../../stores/lib/LocalizedRequest')
);
const config_1 = require('./config');
const defaultReplacer_1 = require('./replacers/defaultReplacer');
const analytics_1 = require('../../analytics');
class DiscreetMode extends feature_1.Feature {
  api;
  analyticsTracker;
  constructor(api, analyticsTracker) {
    super();
    this.api = api;
    this.analyticsTracker = analyticsTracker;
    (0, mobx_1.runInAction)(() => {
      this.getDiscreetModeSettingsRequest = new LocalizedRequest_1.default(
        this.api.getDiscreetModeSettings
      );
      this.setDiscreetModeSettingsRequest = new LocalizedRequest_1.default(
        this.api.setDiscreetModeSettings
      );
    });
  }
  isDiscreetMode = false;
  openInDiscreetMode = false;
  getDiscreetModeSettingsRequest;
  setDiscreetModeSettingsRequest;
  async start() {
    super.start();
    await this._setupDiscreetMode();
  }
  _setupDiscreetMode = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getDiscreetModeSettingsRequest.execute();
    const isDiscreetModeEnabled = this.getDiscreetModeSettingsRequest.result;
    (0, mobx_1.runInAction)('Initialize discreet mode variables', () => {
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Promise<boolean>' is not assignable to type ... Remove this comment to see the full error message
      this.openInDiscreetMode = isDiscreetModeEnabled;
      // @ts-ignore ts-migrate(2322) FIXME: Type 'Promise<boolean>' is not assignable to type ... Remove this comment to see the full error message
      this.isDiscreetMode = isDiscreetModeEnabled;
    });
  };
  toggleDiscreetMode = () => {
    this.isDiscreetMode = !this.isDiscreetMode;
    this.analyticsTracker.sendEvent(
      analytics_1.EventCategories.SETTINGS,
      `Turned ${this.isDiscreetMode ? 'on' : 'off'} discreet mode`
    );
  };
  toggleOpenInDiscreetMode = async () => {
    const nextSetting = !this.openInDiscreetMode;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setDiscreetModeSettingsRequest.execute(nextSetting);
    (0, mobx_1.runInAction)('Update open in discreet mode settings', () => {
      this.openInDiscreetMode = nextSetting;
    });
    this.analyticsTracker.sendEvent(
      analytics_1.EventCategories.SETTINGS,
      `Turned ${nextSetting ? 'on' : 'off'} discreet mode by default`
    );
  };
  discreetValue({
    replacer = (0, defaultReplacer_1.defaultReplacer)(),
    value,
  }) {
    return replacer(this.isDiscreetMode, config_1.SENSITIVE_DATA_SYMBOL, value);
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  DiscreetMode.prototype,
  'isDiscreetMode',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  DiscreetMode.prototype,
  'openInDiscreetMode',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  DiscreetMode.prototype,
  'getDiscreetModeSettingsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  DiscreetMode.prototype,
  'setDiscreetModeSettingsRequest',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  DiscreetMode.prototype,
  'toggleDiscreetMode',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  DiscreetMode.prototype,
  'toggleOpenInDiscreetMode',
  void 0
);
exports.DiscreetMode = DiscreetMode;
//# sourceMappingURL=feature.js.map
