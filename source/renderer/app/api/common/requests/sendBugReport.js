// @flow
import moment from 'moment';
import url from 'url';
import { request } from '../../utils/reportRequest';
import environment from '../../../../../common/environment';

export type SendBugReportParams = {
  requestFormData: {
    email: string,
    subject: string,
    problem: string,
    compressedLogsFile: string,
  },
};

export const sendBugReport = (
  { requestFormData }: SendBugReportParams
) => {
  const { email, subject, problem, compressedLogsFile } = requestFormData;
  const { version, os, API_VERSION, NETWORK, build, getInstallerVersion, REPORT_URL } = environment;
  const reportUrl = url.parse(REPORT_URL);
  const { hostname, port } = reportUrl;

  // Report server recognizes the following networks: mainnet, staging and testnet
  const network = NETWORK === 'development' ? 'staging' : NETWORK;

  return request({
    hostname,
    method: 'POST',
    path: '/api/v1/report',
    port,
  }, {
    product: 'Daedalus Wallet',
    frontendVersion: version,
    backendVersion: API_VERSION,
    network,
    build,
    installerVersion: getInstallerVersion(),
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
