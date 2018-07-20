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
) => {
  const { email, subject, problem, compressedLogsFile } = requestFormData;
  const { version, os, buildNumber, REPORT_URL } = environment;
  const reportUrl = url.parse(REPORT_URL);
  const { hostname, port } = reportUrl;

  return request({
    hostname,
    method: 'POST',
    path: '/report',
    port,
  }, {
    application,
    version,
    build: buildNumber,
    os,
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
