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
  @observable currencyIsFetchingList: boolean = false;
  @observable currencyIsFetchingRate: boolean = false;
  @observable currencyIsAvailable: boolean = false;
  @observable currencyIsActive: boolean = false;
  @observable currencyList: Array<Currency> = [];
  @observable currencySelected: ?Currency = null;
  @observable currencyRate: ?number = null;
  @observable currencyLastFetched: ?Date = null;
  _getCurrencyRateInterval: ?IntervalID = null;

  setup() {
    const { currency: currencyActions } = this.actions;
    currencyActions.setCurrencySelected.listen(this._setCurrencySelected);
    currencyActions.toggleCurrencyIsActive.listen(this._toggleCurrencyIsActive);

    this._setupCurrency();
  }

  // PUBLIC

  @computed get localizedCurrencyList(): Array<LocalizedCurrency> {
    const { currencyList, stores } = this;
    const { currentLocale } = stores.profile;
    return getLocalizedCurrenciesList(currencyList, currentLocale);
  }

  @computed get localizedCurrency(): ?LocalizedCurrency {
    const { currencySelected, stores } = this;
    const { currentLocale } = stores.profile;
    if (!currencySelected) return null;
    return getLocalizedCurrency(currencySelected, currentLocale);
  }

  @action getCurrencyList = async () => {
    this.currencyIsFetchingList = true;
    const currencyList = await this.api.ada.getCurrencyList();
    runInAction(() => {
      this.currencyList = currencyList;
      this.currencyIsFetchingList = false;
    });
  };

  @action getCurrencyRate = async () => {
    const { localizedCurrency } = this;
    if (localizedCurrency && localizedCurrency.code) {
      try {
        this.currencyIsFetchingRate = true;
        const currencyRate = await this.api.ada.getCurrencyRate(
          localizedCurrency
        );
        runInAction(() => {
          this.currencyIsFetchingRate = false;
          this.currencyLastFetched = new Date();
          if (currencyRate) {
            this.currencyRate = currencyRate;
            this.currencyIsAvailable = true;
          } else {
            throw new Error('Error fetching the Currency rate');
          }
        });
      } catch (error) {
        runInAction(() => {
          this.currencyRate = null;
          this.currencyIsAvailable = false;
        });
        clearInterval(this._getCurrencyRateInterval);
      }
    }
  };

  // PRIVATE

  @action _setupCurrency = async () => {
    // Check if the user has enabled currencies
    // Otherwise applies the default config
    const currencyIsActive = await this.api.localStorage.getCurrencyIsActive();

    // Check if the user has already selected a currency
    // Otherwise applies the default currency
    const localCurrencyCode = await this.api.localStorage.getCurrencySelected();
    const currencySelected = getCurrencyFromCode(localCurrencyCode);

    runInAction(() => {
      this.currencyIsActive = currencyIsActive;
      this.currencySelected = currencySelected;
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

  @action _setCurrencySelected = async ({
    currencyCode,
  }: {
    currencyCode: string,
  }) => {
    const { currencyList } = this;
    const currencySelected = currencyList.find(
      ({ code }) => currencyCode === code
    );
    if (currencySelected) {
      this.currencySelected = currencySelected;
      this.getCurrencyRate();
      await this.api.localStorage.setCurrencySelected(currencySelected.code);
    }
  };

  @action _toggleCurrencyIsActive = () => {
    this.currencyIsActive = !this.currencyIsActive;
    this.api.localStorage.setCurrencyIsActive(this.currencyIsActive);
  };
}
