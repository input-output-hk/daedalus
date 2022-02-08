// @flow
import React, { useState } from 'react';
import classnames from 'classnames';
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
  hasTadaIcon?: boolean,
};

const DiscreetToggleTopBar = ({ intl, hasTadaIcon }: Props) => {
  const { isDiscreetMode, toggleDiscreetMode } = useDiscreetModeFeature();
  const [visible, setVisible] = useState(false);
  return (
    <div
      className={classnames(styles.root, hasTadaIcon && styles.hasTadaIcon)}
      onMouseEnter={() => setVisible(true)}
      onMouseLeave={() => setVisible(false)}
    >
      <PopOver
        appendTo="parent"
        visible={visible}
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
          onToggle={toggleDiscreetMode}
        />
      </PopOver>
    </div>
  );
};

export default injectIntl(observer(DiscreetToggleTopBar));
