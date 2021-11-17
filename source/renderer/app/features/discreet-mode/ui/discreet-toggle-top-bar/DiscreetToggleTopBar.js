// @flow
import React from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { injectIntl } from 'react-intl';
import styles from './DiscreetToggleTopBar.scss';
import { messages } from './DiscreetToggleTopBar.messages';
import { useDiscreetModeFeature } from '../../index';
import DiscreetToggle from '../discreet-toggle/DiscreetToggle';
import type { Intl } from '../../../../types/i18nTypes';

type Props = {
  intl: Intl,
  hasTadaIcon?: boolean,
};

const DiscreetToggleTopBar = ({ intl, hasTadaIcon }: Props) => {
  const discreetModeFeature = useDiscreetModeFeature();

  return (
    <div className={classnames(styles.root, hasTadaIcon && styles.hasTadaIcon)}>
      <PopOver
        content={
          <span className={styles.tooltip}>
            {intl.formatMessage(
              messages[
                discreetModeFeature.isDiscreetMode
                  ? 'discreetModeOff'
                  : 'discreetModeOn'
              ]
            )}
          </span>
        }
      >
        <DiscreetToggle
          className={styles.discreetToggle}
          isDiscreetMode={discreetModeFeature.isDiscreetMode}
          onToggle={discreetModeFeature.toggleDiscreetMode}
        />
      </PopOver>
    </div>
  );
};

export default injectIntl(observer(DiscreetToggleTopBar));
