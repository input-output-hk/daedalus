import React from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import hideIcon from '../../../../assets/images/hide-key.inline.svg';
import revealIcon from '../../../../assets/images/reveal-key.inline.svg';
import styles from './DiscreetToggle.scss';

export const DiscreetToggle = ({
  className,
  isDiscreetMode,
  onToggle,
}: {
  className?: string;
  isDiscreetMode: boolean;
  onToggle: () => void;
}) => {
  return (
    <button
      className={classNames(styles.root, className)}
      onClick={onToggle}
      aria-label="discreetModeToggle"
    >
      <SVGInline
        svg={isDiscreetMode ? hideIcon : revealIcon}
        className={classNames(styles.icon, isDiscreetMode && styles.hideIcon)}
      />
    </button>
  );
};
