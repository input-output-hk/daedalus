// @flow
import Action from './lib/Action';

// ======= CURRENCY ACTIONS =======

export default class CurrencyActions {
  setCurrencySelected: Action<{ currencyCode: string }> = new Action();
  toggleCurrencyIsActive: Action<any> = new Action();
}
