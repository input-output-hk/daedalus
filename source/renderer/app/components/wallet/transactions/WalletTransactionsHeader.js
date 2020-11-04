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

const fileContent = [
  ['id', 'amount'],
  ['1', '1.000000'],
  ['2', '2.000000'],
  ['3', '3.000000'],
  ['4', '4.000000'],
];

type Props = {
  children: Node,
  isScrolling: boolean,
  onFilterDialogOpen: Function,
  onRequestCSVFile: Function,
  numberOfTransactions: number,
  numberOfFilterDimensionsApplied: number,
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
      isScrolling,
      onFilterDialogOpen,
      onRequestCSVFile,
      numberOfTransactions,
      numberOfFilterDimensionsApplied,
      /* hasAny, */
    } = this.props;
    const hasAny = true;

    const componentClassnames = classnames([
      styles.component,
      isScrolling ? styles.isScrolling : null,
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
              onClick={() => onRequestCSVFile(fileContent)}
              containerClassName={styles.csvButtonContainer}
              className={styles.csvButton}
              loading={false}
            />
            <FilterButton
              numberOfFilterDimensionsApplied={numberOfFilterDimensionsApplied}
              faded={isScrolling}
              onClick={onFilterDialogOpen}
            />
          </div>
        )}
        {children}
      </div>
    );
  }
}
