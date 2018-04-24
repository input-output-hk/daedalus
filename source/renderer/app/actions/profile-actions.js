// @flow
import Action from './lib/Action';

// ======= PROFILE ACTIONS =======

export default class ProfileActions {
  acceptTermsOfUse: Action<any> = new Action();
  compressLogs: Action<{ logs: Object }> = new Action();
  getLogs: Action<any> = new Action();
  downloadLogs: Action<{ destination: string }> = new Action();
  deleteCompressedLogs: Action<any> = new Action();
  resetBugReportDialog: Action<any> = new Action();
  sendBugReport: Action<{
    email: string, subject: string, problem: string, compressedLog: ?string,
  }> = new Action();
  updateLocale: Action<{ locale: string }> = new Action();
  updateTheme: Action<{ theme: string }> = new Action();
}
