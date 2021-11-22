// @flow
import React from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { injectIntl } from 'react-intl';
import classNames from 'classnames';
import revealIcon from '../../../../assets/images/reveal-key.inline.svg';
import hideIcon from '../../../../assets/images/hide-key.inline.svg';
import { useDiscreetModeFeature } from '../../context';
import styles from './DiscreetToggle.scss';

export const DiscreetToggleButton = ({
  className,
  isDiscreetMode,
  onToggle,
}: {
  className?: string,
  isDiscreetMode: boolean,
  onToggle: () => void,
}) => {
  return (
    <button className={classNames(styles.root, className)} onClick={onToggle}>
      <SVGInline
        svg={isDiscreetMode ? hideIcon : revealIcon}
        className={classNames(styles.icon, isDiscreetMode && styles.hideIcon)}
      />
    </button>
  );
};

type Props = {
  className: string,
};

const DiscreetToggleContainer = ({ className }: Props) => {
  const { isDiscreetMode, toggleDiscreetMode } = useDiscreetModeFeature();
  return (
    <DiscreetToggleButton
      className={className}
      isDiscreetMode={isDiscreetMode}
      onToggle={toggleDiscreetMode}
    />
  );
};

export default injectIntl(observer(DiscreetToggleContainer));
