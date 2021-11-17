// @flow
import React, { useState } from 'react';
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
  intl: Intl,
  hasTadaIcon?: boolean,
};

const DiscreetToggleTopBar = ({ intl, hasTadaIcon }: Props) => {
  const {
    isDiscreetMode,
    isSettingsTooltipEnabled,
    toggleDiscreetMode,
    setDiscreetModeSettingsTooltip,
  } = useDiscreetModeFeature();
  const [visible, setVisible] = useState(false);

  return (
    <div
      className={classnames(styles.root, hasTadaIcon && styles.hasTadaIcon)}
      onMouseEnter={() => setVisible(true)}
      onMouseLeave={() => setVisible(false)}
    >
      <PopOver
        visible={visible || isSettingsTooltipEnabled}
        content={
          <span className={styles.tooltip}>
            {intl.formatMessage(
              messages[isDiscreetMode ? 'discreetModeOff' : 'discreetModeOn']
            )}
          </span>
        }
      >
        <DiscreetModeToggleComponent
          className={styles.discreetToggle}
          isDiscreetMode={isDiscreetMode}
          onToggle={() => {
            toggleDiscreetMode();
            if (isSettingsTooltipEnabled) {
              setDiscreetModeSettingsTooltip(false);
            }
          }}
        />
      </PopOver>
    </div>
  );
};

export default injectIntl(observer(DiscreetToggleTopBar));
