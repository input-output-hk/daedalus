import React from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DiscreetToggleTopBar.scss' o... Remove this comment to see the full error message
import styles from './DiscreetToggleTopBar.scss';
import { useDiscreetModeFeature } from '../../context';
import { DiscreetModeToggleComponent } from '../discreet-toggle/DiscreetModeToggle';

type Props = {
  hasTadaIcon?: boolean;
};

const DiscreetToggleTopBar = ({ hasTadaIcon }: Props) => {
  const { isDiscreetMode, toggleDiscreetMode } = useDiscreetModeFeature();
  return (
    <div className={classnames(styles.root, hasTadaIcon && styles.hasTadaIcon)}>
      <DiscreetModeToggleComponent
        className={styles.discreetToggle}
        isDiscreetMode={isDiscreetMode}
        onToggle={toggleDiscreetMode}
      />
    </div>
  );
};

export default observer(DiscreetToggleTopBar);
