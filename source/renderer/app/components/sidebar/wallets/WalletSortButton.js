// @flow
import React from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import type { WalletSortOrderOptions } from '../../../types/sidebarTypes';
import { WalletSortOrder } from '../../../types/sidebarTypes';
import arrow from '../../../assets/images/sort-arrow.inline.svg';
import styles from './WalletSortButton.scss';

type Props = {
  onClick: () => void,
  label: string,
  isActive: boolean,
  sortOrder: WalletSortOrderOptions,
  tooltip: string,
};

export function WalletSortButton({
  onClick,
  label,
  isActive,
  sortOrder,
  tooltip,
}: Props) {
  const walletSortButtonStyles = classNames([
    styles.walletSortButton,
    isActive ? styles.walletSortButtonActive : null,
  ]);

  const walletSortOrderArrowStyles = classNames([
    styles.walletSortOrderArrowContainer,
    sortOrder === WalletSortOrder.Asc ? styles.walletSortOrderArrowAsc : null,
  ]);

  return (
    <PopOver content={tooltip}>
      <div className={styles.walletSortButtonContainer}>
        <Button
          className={walletSortButtonStyles}
          onClick={onClick}
          label={
            <>
              {isActive ? (
                <div className={walletSortOrderArrowStyles}>
                  <SVGInline
                    svg={arrow}
                    className={styles.walletSortOrderArrow}
                  />
                </div>
              ) : null}
              {label}
            </>
          }
        />
      </div>
    </PopOver>
  );
}
