// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
// import classNames from 'classnames';
// import SVGInline from 'react-svg-inline';
import styles from './BlockConsolidationStatus.scss';
import TopBar from '../layout/TopBar';

type Props = {
};

type State = {
};

@observer
export default class BlockConsolidationStatus extends Component<Props, State> {

  render() {

    return (
      <div className={styles.component}>
        <TopBar
          showSubMenuToggle={false}
        />
        <div className={styles.container}>
          <h1>Block consolidation status</h1>
          <p>
            Block consolidation process never ends,
            it is always 2 epochs behind <br /> the latest epoch.
            Learn more about this process.
          </p>
        </div>
      </div>
    );
  }
}
