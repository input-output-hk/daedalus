// @flow

// 'TextEncoder' is used to measure correct length of UTF-8 strings
export const getContentLength = (content: string) => (
  (new TextEncoder()).encode(content).length
);
