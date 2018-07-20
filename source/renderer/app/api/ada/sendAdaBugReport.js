// @flow
import moment from 'moment';
import url from 'url';
import { request } from '../lib/reportRequest';
import environment from '../../../../common/environment';

export type SendAdaBugReportRequestParams = {
  requestFormData: {
    email: string,
    subject: string,
    problem: string,
    compressedLogsFile: string,
  },
  application: string,
};

export const sendAdaBugReport = (
  { requestFormData, application }: SendAdaBugReportRequestParams
): Promise<{}> => {
  const { email, subject, problem, compressedLogsFile } = requestFormData;
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
    version: environment.version,
    build: environment.build,
    os: platform,
    compressedLogsFile,
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
