import timeMachine from 'timemachine';

const TIME_MACHINE_SESSION_STORAGE_KEY = 'time_machine_date';

export class PersistentTimeMachine {
  private _isInitialized = false;

  init() {
    const dateString = sessionStorage.getItem(TIME_MACHINE_SESSION_STORAGE_KEY);
    timeMachine.config({
      dateString,
    });
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
    window.location.reload();
  }

  private _ensureIsInitialized() {
    if (!this._isInitialized) {
      throw new Error('Time machine must be initialized before use');
    }
  }
}
