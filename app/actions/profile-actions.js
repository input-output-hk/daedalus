// @flow
import Action from './lib/Action';

// ======= PROFILE ACTIONS =======

export default class ProfileActions {
  acceptTermsOfUse: Action<any> = new Action();
  setSendLogsChoice: Action<{ sendLogs: boolean }> = new Action();
  updateLocale: Action<{ locale: string }> = new Action();
}
