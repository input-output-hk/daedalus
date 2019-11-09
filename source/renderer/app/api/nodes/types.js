// @flow
export type NodeSoftware = {
  applicationName: string,
  version: number,
};

// req/res Node Types

export type GetLatestAppVersionResponse = {
  latestAppVersion: ?string,
  applicationVersion: ?number,
};

export type Platform = {
  hash: string,
  SHA256: string,
  signature: string,
  version: string,
  URL: string,
};

export type Platforms = {
  ['windows' | 'darwin' | 'linux']: Platform,
};

export type DaedalusLatestVersionResponse = {
  linux: string,
  linuxHash: string,
  linuxSHA256: string,
  linuxSignature: string,
  linuxURL: string,
  macos: string,
  macosHash: string,
  macosSHA256: string,
  macosSignature: string,
  macosURL: string,
  platforms: Platforms,
  release_notes: ?string,
  win64: string,
  win64Hash: string,
  win64SHA256: string,
  win64Signature: string,
  win64URL: string,
};

export type LatestAppVersionInfoResponse = DaedalusLatestVersionResponse;
