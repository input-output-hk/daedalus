// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import type { Addresses } from '../../../../api/addresses/types';
import styles from './SimpleAddressesList.scss';

type Props = {
  rows: Addresses,
  renderRow: Function,
  showUsed: boolean,
};

@observer
export class SimpleAddressesList extends Component<Props> {

  render() {
    const { rows, renderRow, showUsed } = this.props;
    return (
      <Fragment>
        {
          rows.map((address, index) => {
            const isAddressVisible = !address.used || showUsed;
            if (!isAddressVisible) return null;
            return (
              <div key={index} className={styles.address}>
                { renderRow(address, index) }
              </div>
            );
          })
        }
      </Fragment>
    );
  }
}
