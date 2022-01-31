import React, { useState } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { injectIntl, FormattedHTMLMessage } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DiscreetToggleTopBar.scss' o... Remove this comment to see the full error message
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
      className={classnames(styles.root, hasTadaIcon && styles.hasTadaIcon)}
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
