import React from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { injectIntl } from 'react-intl';
import styles from './DiscreetToggleTopBar.scss';
import { messages } from './DiscreetToggleTopBar.messages';
import { useDiscreetModeFeature } from '../../context';
import { DiscreetModeToggleComponent } from '../discreet-toggle/DiscreetModeToggle';
import type { Intl } from '../../../../types/i18nTypes';

type Props = {
  intl: Intl;
  hasTadaIcon?: boolean;
};

const DiscreetToggleTopBar = ({ intl, hasTadaIcon }: Props) => {
  const { isDiscreetMode, toggleDiscreetMode } = useDiscreetModeFeature();
  return (
    <div className={classnames(styles.root, hasTadaIcon && styles.hasTadaIcon)}>
      <PopOver
        appendTo="parent"
        delay={[300, 0]}
        offset={[0, 0]}
        content={
          <span className={styles.tooltip}>
            {intl.formatMessage(messages[isDiscreetMode ? 'off' : 'on'])}
          </span>
        }
      >
        <DiscreetModeToggleComponent
          className={styles.discreetToggle}
          isDiscreetMode={isDiscreetMode}
          onToggle={toggleDiscreetMode}
        />
      </PopOver>
    </div>
  );
};

export default injectIntl(observer(DiscreetToggleTopBar));
