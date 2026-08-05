import Action from './lib/Action'; // ======= NETWORK STATUS ACTIONS =======

export default class NetworkStatusActions {
  isSyncedAndReady: Action<any> = new Action();
  toggleSplash: Action<any> = new Action();
  forceCheckNetworkClock: Action<any> = new Action();
  toggleRTSFlagsMode: Action<any> = new Action();
}
