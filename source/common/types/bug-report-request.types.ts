export type BugReportRequestHttpOptions = {
  hostname?: string;
  method: string;
  path: string;
  port?: number;
  headers?: {
    'Content-Type': unknown;
  };
};
export type BugReportRequestPayload = {
  product: string;
  frontendVersion: string;
  backendVersion: string;
  network: string;
  build: string;
  installerVersion: string;
  os: string;
  compressedLogsFile: string;
  date: string;
  magic: number;
  type: {
    type: string;
    email: string;
    subject: string;
    problem: string;
  };
};
