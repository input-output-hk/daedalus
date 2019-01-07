// @flow
export type ReportRequestHttpOptions = {
  hostname: ?string,
  method: string,
  path: string,
  port: ?string,
  headers?: {
    'Content-Type': string,
  },
};

export type ReportRequestPayload = {
  product: string,
  frontendVersion: string,
  backendVersion: string,
  network: string,
  build: string,
  installerVersion: string,
  os: string,
  compressedLogsFile: string,
  date: string,
  magic: number,
  type: {
    type: string,
    email: string,
    subject: string,
    problem: string,
  }
};
