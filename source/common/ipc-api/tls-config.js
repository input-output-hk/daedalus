// @flow
export const TLS_CONFIG_CHANNEL = 'TLS_CONFIG';
export type TlsConfig = {
  port: ?number,
  ca: ?Uint8Array,
  cert: ?Uint8Array,
  key: ?Uint8Array,
};
