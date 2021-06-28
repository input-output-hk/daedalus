// @flow
export type RequestConfig = $Exact<{
  hostname: string,
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
}>;

export type MessageDescriptor = {
  id: string,
  description?: string,
  defaultMessage?: string,
};
