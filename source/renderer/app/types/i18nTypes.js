// @flow
export type ReactIntlMessage = {
  id: string,
  defaultMessage: string,
  description: string,
};

export type Intl = {
  formatMessage: (message: ReactIntlMessage) => string,
};
