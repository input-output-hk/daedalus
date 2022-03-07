// @flow
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { useDiscreetModeFeature } from '../context';
import { DiscreetMode } from '../feature';

type Props = {
  children: (discreetModeFeature: DiscreetMode) => Node,
};

const DiscreetModeFeatureInject = ({ children }: Props) => {
  const discreetModeFeature = useDiscreetModeFeature();

  return children(discreetModeFeature);
};

export default observer(DiscreetModeFeatureInject);
