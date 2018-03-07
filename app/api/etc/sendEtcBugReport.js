// @flow
import moment from 'moment';
import url from 'url';
import { request } from '../lib/reportRequest';
import environment from '../../environment';

export type SendEtcBugReportRequestParams = {
  requestFormData: {
    email: string,
    subject: string,
    problem: string,
    compressedLog: string,
  },
  application: string,
};

export const sendEtcBugReport = (
  { requestFormData, application }: SendEtcBugReportRequestParams
): Promise<{}> => {
  const { email, subject, problem, compressedLog } = requestFormData;
  const reportUrl = url.parse(environment.REPORT_URL);

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
    application,
    version: '0.0.1',
    build: environment.build,
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
