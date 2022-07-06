import React from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { injectIntl, FormattedHTMLMessage } from 'react-intl';
import styles from './DiscreetToggleTopBar.scss';
import { messages } from './DiscreetToggleTopBar.messages';
import { useDiscreetModeFeature } from '../../context';
import { DiscreetModeToggleComponent } from '../discreet-toggle/DiscreetModeToggle';
import type { Intl } from '../../../../types/i18nTypes';
import { TOOLTIP_DELAY } from '../../../../config/timingConfig';

type Props = {
  intl: Intl;
  hasTadaIcon?: boolean;
};

function DiscreetToggleTopBar({ intl, hasTadaIcon }: Props) {
  const { isDiscreetMode, toggleDiscreetMode } = useDiscreetModeFeature();
  return (
    <div className={classnames(styles.root, hasTadaIcon && styles.hasTadaIcon)}>
      <PopOver
        appendTo="parent"
        delay={TOOLTIP_DELAY}
        offset={[0, 0]}
        className={styles.popOverRoot}
        content={
          <span className={styles.popOverContent}>
            {intl.formatMessage(messages[isDiscreetMode ? 'off' : 'on'])}
            {` `}
            <FormattedHTMLMessage {...messages.description} />
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
}

export default injectIntl(observer(DiscreetToggleTopBar));
