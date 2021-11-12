// @flow
import React from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { intlShape, injectIntl } from 'react-intl';
import revealIcon from '../../../assets/images/reveal-key.inline.svg';
import hideIcon from '../../../assets/images/hide-key.inline.svg';
import styles from './DiscreetToggle.scss';
import { messages } from './DiscreetToggle.messages';

type Props = {
  className?: string,
  intl: intlShape.isRequired,
  isDiscreetMode?: Boolean,
  onToggle: Function,
};

const DiscreetToggle = ({
  className,
  intl,
  isDiscreetMode = true,
  onToggle,
}: Props) => {
  return (
    <div className={classNames(styles.root, className)}>
      <PopOver
        content={
          <span className={styles.tooltip}>
            {intl.formatMessage(
              messages[isDiscreetMode ? 'discreetModeOff' : 'discreetModeOn']
            )}
          </span>
        }
      >
        <button className={styles.button} onClick={onToggle}>
          <SVGInline
            svg={isDiscreetMode ? hideIcon : revealIcon}
            className={classNames(
              styles.icon,
              isDiscreetMode && styles.hideIcon
            )}
          />
        </button>
      </PopOver>
    </div>
  );
};

export default injectIntl(DiscreetToggle);
