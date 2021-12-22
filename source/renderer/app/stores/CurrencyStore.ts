import { action, observable, computed, runInAction } from 'mobx';
import Store from './lib/Store';
import {
  CURRENCY_REQUEST_RATE_INTERVAL,
  getLocalizedCurrency,
  getLocalizedCurrenciesList,
  getCurrencyFromCode,
} from '../config/currencyConfig';
import type { Currency, LocalizedCurrency } from '../types/currencyTypes';

export default class CurrencyStore extends Store {
  @observable
  isFetchingList = false;
  @observable
  isFetchingRate = false;
  @observable
  isActive = false;
  @observable
  list: Array<Currency> = [];
  @observable
  selected: Currency | null | undefined = null;
  @observable
  rate: number | null | undefined = null;
  @observable
  lastFetched: Date | null | undefined = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _getCurrencyRateInterval: IntervalID | null | undefined = null;

  setup() {
    const { currency: currencyActions } = this.actions;
    currencyActions.setCurrencySelected.listen(this._setCurrencySelected);
    currencyActions.toggleCurrencyIsActive.listen(this._toggleCurrencyIsActive);

    this._setupCurrency();
  }

  // PUBLIC
  @computed
  get localizedCurrencyList(): Array<LocalizedCurrency> {
    const { list, stores } = this;
    const { currentLocale } = stores.profile;
    return getLocalizedCurrenciesList(list, currentLocale);
  }

  @computed
  get localizedCurrency(): LocalizedCurrency | null | undefined {
    const { selected, stores } = this;
    const { currentLocale } = stores.profile;
    if (!selected) return null;
    return getLocalizedCurrency(selected, currentLocale);
  }

  @action
  getCurrencyList = async () => {
    this.isFetchingList = true;
    const list = await this.api.ada.getCurrencyList();
    runInAction(() => {
      this.list = list;
      this.isFetchingList = false;
    });
  };
  @action
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
  @action
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
  @action
  _setCurrencySelected = async ({ code }: { code: string }) => {
    const { list } = this;
    const selected = list.find((item) => item.code === code);

    if (selected) {
      this.selected = selected;
      this.getCurrencyRate();
      await this.api.localStorage.setCurrencySelected(selected.code);
    }
  };
  @action
  _toggleCurrencyIsActive = () => {
    this.isActive = !this.isActive;
    this.api.localStorage.setCurrencyIsActive(this.isActive);
  };
}
