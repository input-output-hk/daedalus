// @flow
import Action from './lib/Action';

// ======= PROFILE ACTIONS =======

export default class ProfileActions {
  acceptTermsOfUse: Action<any> = new Action();
  startDataLayerMigration: Action<any> = new Action();
  getLogs: Action<any> = new Action();
  getLogsAndCompress: Action<any> = new Action();
  sendBugReport: Action<{
    email: string, subject: string, problem: string, compressedLogsFile: ?string,
  }> = new Action();
  resetBugReportDialog: Action<any> = new Action();
  downloadLogs: Action<{ fileName: string, destination: string, fresh?: boolean }> = new Action();
  updateLocale: Action<{ locale: string }> = new Action();
  updateTheme: Action<{ theme: string }> = new Action();
}
