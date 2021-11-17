// @flow
import React from 'react';
import { observer } from 'mobx-react';
import type { Node } from 'react';

import { useDiscreetModeFeature } from '../context';
import { SENSITIVE_DATA_SYMBOL } from '../config';

type Props = {
  children: Node,
};

function DiscreetValue({ children }: Props) {
  const feature = useDiscreetModeFeature();

  return <>{feature.isDiscreetMode ? SENSITIVE_DATA_SYMBOL : children}</>;
}

export default observer(DiscreetValue);
