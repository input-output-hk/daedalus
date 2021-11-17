// @flow
import React from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import revealIcon from '../../../../assets/images/reveal-key.inline.svg';
import hideIcon from '../../../../assets/images/hide-key.inline.svg';
import styles from './DiscreetToggle.scss';

type Props = {
  className?: string,
  isDiscreetMode?: boolean,
  onToggle: Function,
};

const DiscreetToggle = ({
  className,
  isDiscreetMode = true,
  onToggle,
}: Props) => {
  return (
    <button className={classNames(styles.root, className)} onClick={onToggle}>
      <SVGInline
        svg={isDiscreetMode ? hideIcon : revealIcon}
        className={classNames(styles.icon, isDiscreetMode && styles.hideIcon)}
      />
    </button>
  );
};

export default DiscreetToggle;
