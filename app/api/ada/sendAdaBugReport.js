// @flow
import moment from 'moment';
import { request } from './lib/reportRequest';
import environment from '../../environment';

export type SendAdaBugReportRequestParams = {
  requestFormData: {
    email: string,
    subject: string,
    problem: string,
    logs: Array<string>,
  },
};

export const sendAdaBugReport = (
  { requestFormData }: SendAdaBugReportRequestParams
): Promise<{}> => {
  const { email, subject, problem, logs } = requestFormData;

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
    hostname: 'localhost',
    method: 'POST',
    path: '/report',
    port: 8000,
  }, {
    application: 'cardano-node',
    version: '0.0.1',
    build: environment.build,
    os: platform,
    logs,
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
