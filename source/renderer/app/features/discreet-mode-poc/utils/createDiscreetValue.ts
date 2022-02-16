import { SENSITIVE_DATA_SYMBOL } from '../config';
import { ReplacerFn } from '../replacers';
import { defaultReplacer } from '../replacers/defaultReplacer';

export function createDiscreetValue({
  isDiscreetMode,
  replacer = defaultReplacer(),
  value,
  sensitiveDataSymbol = SENSITIVE_DATA_SYMBOL,
}: {
  isDiscreetMode: boolean;
  replacer?: ReplacerFn;
  value?: number | string;
  sensitiveDataSymbol?: unknown;
}) {
  return replacer(isDiscreetMode, sensitiveDataSymbol, value);
}
