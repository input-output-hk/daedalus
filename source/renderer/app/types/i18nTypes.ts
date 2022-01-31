export type ReactIntlMessage = {
  id: string;
  defaultMessage: string;
  description: string;
};
// @ts-ignore ts-migrate(2456) FIXME: Type alias 'Record' circularly references itself.
type Record<T, V> = Record<T, V>;
type MessageFormatPrimitiveValue = string | number | boolean | null;
export type Intl = {
  formatMessage: (
    message: ReactIntlMessage,
    // @ts-ignore ts-migrate(2315) FIXME: Type 'Record' is not generic.
    values?: Record<string, MessageFormatPrimitiveValue>
  ) => string;
};
