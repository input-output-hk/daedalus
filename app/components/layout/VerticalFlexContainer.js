// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './VerticalFlexContainer.scss';
import { oneOrManyChildElements } from '../../propTypes';

@observer
export default class VerticalFlexContainer extends Component {

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
