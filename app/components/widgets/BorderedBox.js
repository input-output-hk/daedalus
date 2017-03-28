// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './BorderedBox.scss';
import { oneOrManyChildElements } from '../../propTypes';

@observer
export default class BorderedBox extends Component {

  static propTypes = {
    children: oneOrManyChildElements,
  };

  render() {
    const { children } = this.props;
    return (
      <div className={styles.component}>
        {children}
      </div>
    );
  }
}
