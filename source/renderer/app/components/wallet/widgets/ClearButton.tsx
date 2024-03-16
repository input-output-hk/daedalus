// @ts-nocheck
import React from 'react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import SVGInline from 'react-svg-inline';
import closeIcon from '../../../assets/images/close-cross.inline.svg';
import styles from './ClearButton.scss';

interface Props {
  label: string;
  onClick: () => void;
}

export function ClearButton({ label, onClick }: Props) {
  return (
    <PopOver content={label} placement="top">
      <button onClick={onClick} className={styles.component} tabIndex={-1}>
        <SVGInline svg={closeIcon} className={styles.clearIcon} />
      </button>
    </PopOver>
  );
}
