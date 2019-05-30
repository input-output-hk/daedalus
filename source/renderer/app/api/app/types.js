// @flow
export type Platform = {
  signature: string,
  hash: string,
  URL: string,
  version: string,
  SHA256: string,
};

export type Platforms = {
  ['windows' | 'darwin' | 'linux']: Platform,
};

export type DaedalusLatestVersionResponse = {
  platforms: Platforms,
  linuxHash: string,
  macosSignature: string,
  win64: string,
  linuxSignature: string,
  linuxURL: string,
  macosSHA256: string,
  win64Hash: string,
  macosURL: string,
  win64URL: string,
  linuxSHA256: string,
  release_notes: ?string,
  macos: string,
  win64Signature: string,
  linux: string,
  macosHash: string,
  win64SHA256: string,
};

export type GetLatestAppVersionInfoResponse = DaedalusLatestVersionResponse;
