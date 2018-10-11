// @flow
import moment from 'moment';
import url from 'url';
import { request } from '../../utils/reportRequest';

export type SendBugReportParams = {
  requestFormData: {
    email: string,
    subject: string,
    problem: string,
    compressedLogsFile: string,
  },
  environmentData: {
    network: string,
    version: string,
    os: string,
    apiVersion: string,
    build: string,
    installerVersion: string,
    reportURL: string,
  }
};

export const sendBugReport = (
  { requestFormData, environmentData }: SendBugReportParams
) => {
  const { email, subject, problem, compressedLogsFile } = requestFormData;
  const { version, os, apiVersion, network, build, installerVersion, reportURL } = environmentData;
  const parsedReportURL = url.parse(reportURL);
  const { hostname, port } = parsedReportURL;
  // Report server recognizes the following networks: mainnet, staging and testnet
  const serverNetwork = network === 'development' ? 'staging' : network;

  return request({
    hostname,
    method: 'POST',
    path: '/api/v1/report',
    port,
  }, {
    product: 'Daedalus Wallet',
    frontendVersion: version,
    backendVersion: apiVersion,
    network: serverNetwork,
    build,
    installerVersion,
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
