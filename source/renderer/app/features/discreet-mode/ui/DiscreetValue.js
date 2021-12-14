// @flow
import React from 'react';
import { observer } from 'mobx-react';
import type { Node } from 'react';

import { useDiscreetModeFeature } from '../context';
import { SENSITIVE_DATA_SYMBOL } from '../config';
import { defaultReplacer } from '../replacers/defaultReplacer';
import type { ReplacerFn } from '../types';

type Props = {
  children: Node,
  replacer: ReplacerFn,
};

function DiscreetValue({ children, replacer = defaultReplacer() }: Props) {
  const feature = useDiscreetModeFeature();
  return (
    <>{replacer(feature.isDiscreetMode, SENSITIVE_DATA_SYMBOL, children)}</>
  );
}

export default observer(DiscreetValue);
