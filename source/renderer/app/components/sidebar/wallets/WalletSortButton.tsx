import React from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import { Button } from '@react-polymorph/components/Button';
import { PopOver } from '@react-polymorph/components/PopOver';
import type { WalletSortOrderOptions } from '../../../types/sidebarTypes';
import { WalletSortOrder } from '../../../types/sidebarTypes';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/sort-ar... Remove this comment to see the full error message
import arrow from '../../../assets/images/sort-arrow.inline.svg';
import styles from './WalletSortButton.scss';

type Props = {
  onClick: () => void;
  label: string;
  isActive: boolean;
  sortOrder: WalletSortOrderOptions;
  tooltip: string;
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
