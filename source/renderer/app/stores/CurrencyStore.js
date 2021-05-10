// @flow
import { action, observable, computed, runInAction } from 'mobx';
import Store from './lib/Store';
import {
  CURRENCY_REQUEST_RATE_INTERVAL,
  getLocalizedCurrency,
  getLocalizedCurrenciesList,
  getCurrencyFromCode,
} from '../config/currencyConfig';
import type { Currency, LocalizedCurrency } from '../types/currencyTypes.js';

export default class CurrencyStore extends Store {
  @observable isFetchingList: boolean = false;
  @observable isFetchingRate: boolean = false;
  @observable isAvailable: boolean = false;
  @observable isActive: boolean = false;
  @observable list: Array<Currency> = [];
  @observable selected: ?Currency = null;
  @observable rate: ?number = null;
  @observable lastFetched: ?Date = null;
  _getCurrencyRateInterval: ?IntervalID = null;

  setup() {
    const { currency: currencyActions } = this.actions;
    currencyActions.setCurrencySelected.listen(this._setCurrencySelected);
    currencyActions.toggleCurrencyIsActive.listen(this._toggleCurrencyIsActive);

    this._setupCurrency();
  }

  // PUBLIC

  @computed get localizedCurrencyList(): Array<LocalizedCurrency> {
    const { list, stores } = this;
    const { currentLocale } = stores.profile;
    return getLocalizedCurrenciesList(list, currentLocale);
  }

  @computed get localizedCurrency(): ?LocalizedCurrency {
    const { selected, stores } = this;
    const { currentLocale } = stores.profile;
    if (!selected) return null;
    return getLocalizedCurrency(selected, currentLocale);
  }

  @action getCurrencyList = async () => {
    this.isFetchingList = true;
    const list = await this.api.ada.getCurrencyList();
    runInAction(() => {
      this.list = list;
      this.isFetchingList = false;
    });
  };

  @action getCurrencyRate = async () => {
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
          this.isFetchingRate = false;
          this.lastFetched = new Date();
          this.rate = rate;
          this.isAvailable = true;
        });
      } catch (error) {
        /*
         * In case the currency rate API fetching fails,
         * it tries to use the stored local data
         */
        let rate = null;
        let lastFetched;
        let isAvailable = false;
        const localRate = await this.api.localStorage.getCurrencyRate(
          localizedCurrency.code
        );
        if (localRate) {
          rate = localRate.rate;
          lastFetched = localRate.date;
          isAvailable = true;
        }
        runInAction(() => {
          this.rate = rate;
          this.isAvailable = isAvailable;
          this.lastFetched = lastFetched;
          this.isFetchingRate = false;
        });
      }
    }
  };

  // PRIVATE

  @action _setupCurrency = async () => {
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

  @action _setCurrencySelected = async ({ code }: { code: string }) => {
    const { list } = this;
    const selected = list.find((item) => item.code === code);
    if (selected) {
      this.selected = selected;
      this.getCurrencyRate();
      await this.api.localStorage.setCurrencySelected(selected.code);
    }
  };

  @action _toggleCurrencyIsActive = () => {
    this.isActive = !this.isActive;
    this.api.localStorage.setCurrencyIsActive(this.isActive);
  };

  _setLocalRate = async () => {
    const { localizedCurrency, rate, lastFetched } = this;
    if (localizedCurrency && localizedCurrency.code && rate) {
      const currencyLocalRate = {
        rate,
        date: lastFetched || new Date(),
      };
      this.api.localStorage.setCurrencyRate(
        localizedCurrency.code,
        currencyLocalRate
      );
    }
  };

  onExit() {
    // Save the last fetching to the localStorage
    this._setLocalRate();
  }
}
