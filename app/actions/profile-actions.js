// @flow
import Action from './lib/Action';

// ======= PROFILE ACTIONS =======

export default class ProfileActions {
  acceptTermsOfUse: Action<any> = new Action();
  compressLogs: Action<{ logs: Object }> = new Action();
  getLogs: Action<any> = new Action();
  setSendLogsChoice: Action<{ sendLogs: boolean }> = new Action();
  sendSupportRequest: Action<{
    email: string, subject: ?string, problem: ?string, files: Array<string>,
  }> = new Action();
  updateLocale: Action<{ locale: string }> = new Action();
  updateTheme: Action<{ theme: string }> = new Action();
}
