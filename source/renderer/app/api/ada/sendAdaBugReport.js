// @flow
import moment from 'moment';
import url from 'url';
import { uniq } from 'lodash';
import { request } from '../lib/reportRequest';
import environment from '../../../../common/environment';

export type SendAdaBugReportRequestParams = {
  requestFormData: {
    email: string,
    subject: string,
    problem: string,
    compressedLog: string,
  },
  application: string,
};

export const sendAdaBugReport = (
  { requestFormData, application }: SendAdaBugReportRequestParams
): Promise<{}> => {
  const { email, subject, problem, compressedLog } = requestFormData;
  const { platform, version, build, API_VERSION, REPORT_URL } = environment;
  const buildNumber = uniq([API_VERSION, build]).join('.');
  const reportUrl = url.parse(REPORT_URL);

  let os;
  switch (platform) {
    case 'darwin':
      os = 'macOS';
      break;
    case 'win32':
      os = 'Windows';
      break;
    case 'linux':
      os = 'Linux';
      break;
    default:
      os = '';
  }

  return request({
    hostname: reportUrl.hostname,
    method: 'POST',
    path: '/report',
    port: reportUrl.port,
  }, {
    application,
    version,
    build: buildNumber,
    os,
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
