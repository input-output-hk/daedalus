import {
  action,
  observable,
  computed,
  runInAction,
  makeObservable,
} from 'mobx';
import Store from './lib/Store';
import {
  CURRENCY_REQUEST_RATE_INTERVAL,
  getLocalizedCurrency,
  getLocalizedCurrenciesList,
  getCurrencyFromCode,
} from '../config/currencyConfig';
import type { Currency, LocalizedCurrency } from '../types/currencyTypes';
import { AnalyticsTracker, EventCategories } from '../analytics';
import { Api } from '../api';
import { ActionsMap } from '../actions';

export default class CurrencyStore extends Store {
  isFetchingList = false;
  isFetchingRate = false;
  isActive = false;
  list: Array<Currency> = [];
  selected: Currency | null | undefined = null;
  rate: number | null | undefined = null;
  lastFetched: Date | null | undefined = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _getCurrencyRateInterval: IntervalID | null | undefined = null;

  constructor(api: Api, actions: ActionsMap, analytics: AnalyticsTracker) {
    super(api, actions, analytics);

    makeObservable(this, {
      isFetchingList: observable,
      isFetchingRate: observable,
      isActive: observable,
      list: observable,
      selected: observable,
      rate: observable,
      lastFetched: observable,
      localizedCurrencyList: computed,
      localizedCurrency: computed,
      getCurrencyList: action,
      getCurrencyRate: action,
      _setupCurrency: action,
      _setCurrencySelected: action,
      _toggleCurrencyIsActive: action,
    });
  }

  setup() {
    const { currency: currencyActions } = this.actions;
    currencyActions.setCurrencySelected.listen(this._setCurrencySelected);
    currencyActions.toggleCurrencyIsActive.listen(this._toggleCurrencyIsActive);

    this._setupCurrency();
  }

  // PUBLIC
  get localizedCurrencyList(): Array<LocalizedCurrency> {
    const { list, stores } = this;
    const { currentLocale } = stores.profile;
    return getLocalizedCurrenciesList(list, currentLocale);
  }

  get localizedCurrency(): LocalizedCurrency | null | undefined {
    const { selected, stores } = this;
    const { currentLocale } = stores.profile;
    if (!selected) return null;
    return getLocalizedCurrency(selected, currentLocale);
  }

  getCurrencyList = async () => {
    this.isFetchingList = true;
    const list = await this.api.ada.getCurrencyList();
    runInAction(() => {
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

        runInAction(() => {
          this.lastFetched = new Date();
          this.rate = rate;
          this.isFetchingRate = false;
        });
      } catch (error) {
        runInAction(() => {
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
    const selected = getCurrencyFromCode(localCurrencyCode);
    runInAction(() => {
      this.isActive = isActive;
      this.selected = selected;
    });
    clearInterval(this._getCurrencyRateInterval);
    this._getCurrencyRateInterval = setInterval(
      this.getCurrencyRate,
      CURRENCY_REQUEST_RATE_INTERVAL
    );
    // Fetch the currency list and rate
    this.getCurrencyList();
    this.getCurrencyRate();
  };
  _setCurrencySelected = async ({ code }: { code: string }) => {
    const { list } = this;
    const selected = list.find((item) => item.code === code);

    if (selected) {
      this.selected = selected;
      this.getCurrencyRate();
      await this.api.localStorage.setCurrencySelected(selected.code);
    }

    this.analytics.sendEvent(
      EventCategories.SETTINGS,
      'Changed currency',
      code
    );
  };
  _toggleCurrencyIsActive = () => {
    this.isActive = !this.isActive;
    this.api.localStorage.setCurrencyIsActive(this.isActive);

    this.analytics.sendEvent(
      EventCategories.SETTINGS,
      `Turned ${
        this.isActive ? 'on' : 'off'
      } displaying ada balances in other currency`
    );
  };
}
