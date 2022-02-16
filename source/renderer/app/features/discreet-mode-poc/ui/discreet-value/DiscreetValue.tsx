import React from 'react';
import { SENSITIVE_DATA_SYMBOL } from '../../config';
import { DiscreetMode } from '../../feature';
import { ReplacerFn } from '../../replacers';
import { defaultReplacer } from '../../replacers/defaultReplacer';
import { isDiscreetMode } from '../../selectors';

type Props = {
  children: React.ReactNode;
  replacer?: ReplacerFn;
};

export function DiscreetValue({
  children,
  replacer = defaultReplacer(),
}: Props) {
  return (
    <>
      {replacer(
        DiscreetMode.useSelector(isDiscreetMode),
        SENSITIVE_DATA_SYMBOL,
        children
      )}
    </>
  );
}
