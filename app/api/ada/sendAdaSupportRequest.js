// @flow
import moment from 'moment';
import { request } from './lib/reportRequest';
import environment from '../../environment';
import { APP_NAME } from '../../../electron/config';

export type SendAdaSupportRequestParams = {
  requestFormData: {
    email: string,
    subject: string,
    problem: string,
    logs: Array<string>,
  },
};

export const sendAdaSupportRequest = (
  { requestFormData }: SendAdaSupportRequestParams
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
    headers : {
      'Content-Type': 'application/x-www-form-urlencoded',
    }
  },
  {
    application: 'cardano-node', // if is set to Daedalus then error appears with message that application needs to be in cardano-node
    version: '0.0.1',
    build: environment.build,
    os: platform,
    logs,
    date: moment().format('YYYY-MM-DDTH:m:s'),
    magic: 2000000000,
    type: {
      type : 'customreport',
      email,
      subject,
      problem,
    }
  });
};
