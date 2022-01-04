import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import FilterButton from './FilterButton';
import FilterDialog from './FilterDialog';
import type { FilterDialogProps } from './FilterDialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletTransactionsHeader.scs... Remove this comment to see the full error message
import styles from './WalletTransactionsHeader.scss';
import TinyButton from '../../widgets/forms/TinyButton';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/downloa... Remove this comment to see the full error message
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
  isFilterDisabled: boolean;
  isScrolling: boolean;
  filterDialogProps: FilterDialogProps;
  numberOfFilterDimensionsApplied: number;
  numberOfTransactions: number;
  onRequestCSVFile: (...args: Array<any>) => any;
};

@observer
class WalletTransactionsHeader extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      isFilterDisabled,
      isScrolling,
      filterDialogProps,
      numberOfFilterDimensionsApplied,
      numberOfTransactions,
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
            <FilterDialog
              {...filterDialogProps}
              triggerElement={
                <FilterButton
                  disabled={isFilterDisabled}
                  numberOfFilterDimensionsApplied={
                    numberOfFilterDimensionsApplied
                  }
                />
              }
            />
          </div>
        )}
      </div>
    );
  }
}

export default WalletTransactionsHeader;
