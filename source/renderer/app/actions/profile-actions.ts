import Action from './lib/Action'; // ======= PROFILE ACTIONS =======

export default class ProfileActions {
  acceptTermsOfUse: Action<any> = new Action();
  acceptDataLayerMigration: Action<any> = new Action();
  getLogs: Action<any> = new Action();
  getLogsAndCompress: Action<any> = new Action();
  resetBugReportDialog: Action<any> = new Action();
  downloadLogs: Action<{
    fileName: string;
    destination: string;
    fresh?: boolean;
  }> = new Action();
  downloadLogsSuccess: Action<any> = new Action();
  updateUserLocalSetting: Action<{
    param: string;
    value?: string;
  }> = new Action();
  updateTheme: Action<{
    theme: string;
  }> = new Action();
  finishInitialScreenSettings: Action<any> = new Action();
}
