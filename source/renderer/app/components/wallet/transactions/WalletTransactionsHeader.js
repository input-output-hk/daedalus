// @flow
import React, { Component, Fragment } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import FilterButton from './FilterButton';
import styles from './WalletTransactionsHeader.scss';
import TinyButton from '../../widgets/forms/TinyButton';
import downloadIcon from '../../../assets/images/download-icon.inline.svg';

export const messages = defineMessages({
  transactions: {
    id: 'wallet.transactions.header.transactions',
    defaultMessage: '!!!Transactions',
    description: 'Label for the "Transactions" header.',
  },
  exportCSVButtonLabel: {
    id: 'wallet.transactions.header.exportCSV.button.label',
    defaultMessage: '!!!Export CSV',
    description: 'Label for the "Export CSV" button.',
  },
});

type Props = {
  children: Node,
  isFilterDisabled: boolean,
  isScrolling: boolean,
  numberOfFilterDimensionsApplied: number,
  numberOfTransactions: number,
  onFilterDialogOpen: Function,
  onRequestCSVFile: Function,
};

@observer
export default class WalletTransactionsHeader extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      children,
      isFilterDisabled,
      isScrolling,
      numberOfFilterDimensionsApplied,
      numberOfTransactions,
      onFilterDialogOpen,
      onRequestCSVFile,
    } = this.props;
    const hasAny = true;

    const componentClassnames = classnames([
      styles.component,
      isScrolling ? styles.isScrolling : null,
    ]);

    const isCsvButtonDisabled = numberOfTransactions === 0;

    const cvsButtonClassnames = classnames([
      styles.csvButton,
      isCsvButtonDisabled ? styles.csvButtonDisabled : null,
      'flat',
    ]);

    return (
      <div className={componentClassnames}>
        <div className={styles.numberOfTransactions}>
          {intl.formatMessage(messages.transactions)} ({numberOfTransactions})
        </div>
        {hasAny && (
          <div className={styles.actions}>
            <TinyButton
              label={
                <Fragment>
                  {intl.formatMessage(messages.exportCSVButtonLabel)}
                  <SVGInline
                    svg={downloadIcon}
                    className={styles.downloadIcon}
                  />
                </Fragment>
              }
              onClick={onRequestCSVFile}
              containerClassName={styles.csvButtonContainer}
              className={cvsButtonClassnames}
              disabled={isCsvButtonDisabled}
              loading={false}
            />
            <FilterButton
              disabled={isFilterDisabled}
              numberOfFilterDimensionsApplied={numberOfFilterDimensionsApplied}
              onClick={onFilterDialogOpen}
            />
          </div>
        )}
        {children}
      </div>
    );
  }
}
