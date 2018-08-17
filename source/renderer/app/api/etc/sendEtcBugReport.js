// @flow
import moment from 'moment';
import url from 'url';
import { request } from '../lib/reportRequest';
import environment from '../../../../common/environment';

export type SendEtcBugReportRequestParams = {
  requestFormData: {
    email: string,
    subject: string,
    problem: string,
    compressedLog: string,
  },
};

export const sendEtcBugReport = (
  { requestFormData }: SendEtcBugReportRequestParams
): Promise<{}> => {
  const { email, subject, problem, compressedLog } = requestFormData;
  const { version, API_VERSION, NETWORK, build, getInstallerVersion, REPORT_URL } = environment;
  const reportUrl = url.parse(REPORT_URL);

  let platform;
  switch (environment.platform) {
    case 'darwin':
      platform = 'macOS';
      break;
    case 'win32':
      platform = 'Windows';
      break;
    case 'linux':
      platform = 'Linux';
      break;
    default:
      platform = '';
  }

  return request({
    hostname: reportUrl.hostname,
    method: 'POST',
    path: '/report',
    port: reportUrl.port,
  }, {
    product: 'Mantis Wallet',
    frontendVersion: version,
    backendVersion: API_VERSION,
    network: NETWORK,
    build,
    installerVersion: getInstallerVersion(),
    os: platform,
    compressedLog,
    date: moment().format('YYYY-MM-DDTHH:mm:ss'),
    magic: 2000000000,
    type: {
      type: 'customreport',
      email,
      subject,
      problem,
    }
  });
};
