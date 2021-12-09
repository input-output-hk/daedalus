// @flow
import React, { useState } from 'react';
import { observer } from 'mobx-react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { injectIntl, FormattedHTMLMessage } from 'react-intl';
import styles from './DiscreetToggleTopBar.scss';
import { messages } from './DiscreetToggleTopBar.messages';
import { useDiscreetModeFeature } from '../../context';
import { DiscreetModeToggleComponent } from '../discreet-toggle/DiscreetModeToggle';
import type { Intl } from '../../../../types/i18nTypes';

type Props = {
  intl: Intl,
};

const DiscreetToggleTopBar = ({ intl }: Props) => {
  const {
    isDiscreetMode,
    isSettingsTooltipEnabled,
    toggleDiscreetMode,
    setDiscreetModeSettingsTooltip,
  } = useDiscreetModeFeature();
  const [visible, setVisible] = useState(false);
  const isPopOverVisible = visible || isSettingsTooltipEnabled;

  return (
    <div
      className={styles.root}
      onMouseEnter={() => setVisible(true)}
      onMouseLeave={() => setVisible(false)}
    >
      <PopOver
        appendTo="parent"
        visible={isPopOverVisible}
        className={styles.popOverRoot}
        content={
          <span className={styles.content}>
            {intl.formatMessage(messages[isDiscreetMode ? 'off' : 'on'])}
            {` `}
            <FormattedHTMLMessage {...messages.description} />
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
