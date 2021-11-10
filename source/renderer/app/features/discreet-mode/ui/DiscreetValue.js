// @flow
import React from 'react';
import { observer } from 'mobx-react';
import type { Node } from 'react';

import { useDiscreetModeFeature } from '../index';

type Props = {
  children: Node,
};

function DiscreetValue({ children }: Props) {
  const feature = useDiscreetModeFeature();

  if (feature.isDiscreetMode) {
    return <span>***</span>;
  }

  return <>{children}</>;
}

export default observer(DiscreetValue);
