export type ReactIntlMessage = {
  id: string;
  defaultMessage: string;
  description: string;
};
type Record<T, V> = Record<T, V>;
type MessageFormatPrimitiveValue = string | number | boolean | null;
export type Intl = {
  formatMessage: (
    message: ReactIntlMessage,
    values?: Record<string, MessageFormatPrimitiveValue>
  ) => string;
};
