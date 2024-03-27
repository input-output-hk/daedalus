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
const mobx_1 = require('mobx');
const Store_1 = __importDefault(require('./lib/Store'));
const currencyConfig_1 = require('../config/currencyConfig');
const analytics_1 = require('../analytics');
class CurrencyStore extends Store_1.default {
  isFetchingList = false;
  isFetchingRate = false;
  isActive = false;
  list = [];
  selected = null;
  rate = null;
  lastFetched = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _getCurrencyRateInterval = null;
  setup() {
    const { currency: currencyActions } = this.actions;
    currencyActions.setCurrencySelected.listen(this._setCurrencySelected);
    currencyActions.toggleCurrencyIsActive.listen(this._toggleCurrencyIsActive);
    this._setupCurrency();
  }
  // PUBLIC
  get localizedCurrencyList() {
    const { list, stores } = this;
    const { currentLocale } = stores.profile;
    return (0, currencyConfig_1.getLocalizedCurrenciesList)(
      list,
      currentLocale
    );
  }
  get localizedCurrency() {
    const { selected, stores } = this;
    const { currentLocale } = stores.profile;
    if (!selected) return null;
    return (0, currencyConfig_1.getLocalizedCurrency)(selected, currentLocale);
  }
  getCurrencyList = async () => {
    this.isFetchingList = true;
    const list = await this.api.ada.getCurrencyList();
    (0, mobx_1.runInAction)(() => {
      this.list = list;
      this.isFetchingList = false;
    });
  };
  getCurrencyRate = async () => {
    const { localizedCurrency, list } = this;
    /**
     * In case the list was not fetched when loading,
     * it tries again
     */
    if (!list || !list.length) {
      this.getCurrencyList();
    }
    if (localizedCurrency && localizedCurrency.code) {
      try {
        this.isFetchingRate = true;
        const rate = await this.api.ada.getCurrencyRate(localizedCurrency);
        if (!rate) {
          throw new Error('Error fetching the Currency rate');
        }
        (0, mobx_1.runInAction)(() => {
          this.lastFetched = new Date();
          this.rate = rate;
          this.isFetchingRate = false;
        });
      } catch (error) {
        (0, mobx_1.runInAction)(() => {
          if (this.rate) {
            this.isFetchingRate = false;
          }
        });
      }
    }
  };
  // PRIVATE
  _setupCurrency = async () => {
    // Check if the user has enabled currencies
    // Otherwise applies the default config
    const isActive = await this.api.localStorage.getCurrencyIsActive();
    // Check if the user has already selected a currency
    // Otherwise applies the default currency
    const localCurrencyCode = await this.api.localStorage.getCurrencySelected();
    const selected = (0, currencyConfig_1.getCurrencyFromCode)(
      localCurrencyCode
    );
    (0, mobx_1.runInAction)(() => {
      this.isActive = isActive;
      this.selected = selected;
    });
    clearInterval(this._getCurrencyRateInterval);
    this._getCurrencyRateInterval = setInterval(
      this.getCurrencyRate,
      currencyConfig_1.CURRENCY_REQUEST_RATE_INTERVAL
    );
    // Fetch the currency list and rate
    this.getCurrencyList();
    this.getCurrencyRate();
  };
  _setCurrencySelected = async ({ code }) => {
    const { list } = this;
    const selected = list.find((item) => item.code === code);
    if (selected) {
      this.selected = selected;
      this.getCurrencyRate();
      await this.api.localStorage.setCurrencySelected(selected.code);
    }
    this.analytics.sendEvent(
      analytics_1.EventCategories.SETTINGS,
      'Changed currency',
      code
    );
  };
  _toggleCurrencyIsActive = () => {
    this.isActive = !this.isActive;
    this.api.localStorage.setCurrencyIsActive(this.isActive);
    this.analytics.sendEvent(
      analytics_1.EventCategories.SETTINGS,
      `Turned ${
        this.isActive ? 'on' : 'off'
      } displaying ada balances in other currency`
    );
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  CurrencyStore.prototype,
  'isFetchingList',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  CurrencyStore.prototype,
  'isFetchingRate',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  CurrencyStore.prototype,
  'isActive',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  CurrencyStore.prototype,
  'list',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  CurrencyStore.prototype,
  'selected',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  CurrencyStore.prototype,
  'rate',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Date)],
  CurrencyStore.prototype,
  'lastFetched',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  CurrencyStore.prototype,
  'localizedCurrencyList',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  CurrencyStore.prototype,
  'localizedCurrency',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  CurrencyStore.prototype,
  'getCurrencyList',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  CurrencyStore.prototype,
  'getCurrencyRate',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  CurrencyStore.prototype,
  '_setupCurrency',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  CurrencyStore.prototype,
  '_setCurrencySelected',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  CurrencyStore.prototype,
  '_toggleCurrencyIsActive',
  void 0
);
exports.default = CurrencyStore;
//# sourceMappingURL=CurrencyStore.js.map
