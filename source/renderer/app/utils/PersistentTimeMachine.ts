import timeMachine from 'timemachine';

// https://github.com/schickling/timemachine/issues/8
timeMachine.reset();

const TIME_MACHINE_SESSION_STORAGE_KEY = 'time_machine_date';

export class PersistentTimeMachine {
  private _isInitialized = false;

  init() {
    const dateString = sessionStorage.getItem(TIME_MACHINE_SESSION_STORAGE_KEY);
    if (dateString !== null) {
      timeMachine.config({
        dateString,
      });
    }
    this._isInitialized = true;
  }

  enable(dateString: string) {
    this._ensureIsInitialized();
    timeMachine.config({
      dateString,
    });
    sessionStorage.setItem(TIME_MACHINE_SESSION_STORAGE_KEY, dateString);
    window.location.reload();
  }

  disable() {
    this._ensureIsInitialized();
    timeMachine.reset();
    sessionStorage.removeItem(TIME_MACHINE_SESSION_STORAGE_KEY);
    window.location.reload();
  }

  private _ensureIsInitialized() {
    if (!this._isInitialized) {
      throw new Error('Time machine must be initialized before use');
    }
  }
}
