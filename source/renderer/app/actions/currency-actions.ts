import Action from './lib/Action'; // ======= CURRENCY ACTIONS =======

export default class CurrencyActions {
  setCurrencySelected: Action<{
    code: string;
  }> = new Action();
  toggleCurrencyIsActive: Action<any> = new Action();
}
