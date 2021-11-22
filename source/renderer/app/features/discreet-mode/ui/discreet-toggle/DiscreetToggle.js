// @flow
import React from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import revealIcon from '../../../../assets/images/reveal-key.inline.svg';
import hideIcon from '../../../../assets/images/hide-key.inline.svg';
import { useDiscreetModeFeature } from '../../context';
import styles from './DiscreetToggle.scss';

type Props = {
  className?: string,
};

const DiscreetToggle = ({ className }: Props) => {
  const { isDiscreetMode, toggleDiscreetMode } = useDiscreetModeFeature();
  return (
    <button
      className={classNames(styles.root, className)}
      onClick={toggleDiscreetMode}
    >
      <SVGInline
        svg={isDiscreetMode ? hideIcon : revealIcon}
        className={classNames(styles.icon, isDiscreetMode && styles.hideIcon)}
      />
    </button>
  );
};

export default DiscreetToggle;
