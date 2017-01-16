// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './MnemonicWord.scss';

@observer
export default class MnemonicWord extends Component {

  static propTypes = {
    word: PropTypes.string.isRequired,
    isActive: PropTypes.bool.isRequired
  };

  render() {
    const { word, isActive } = this.props;
    const componentClassNames = classnames([
      styles.component,
      isActive ? styles.active : styles.inactive
    ]);
    return (
      <div className={componentClassNames}>{word}</div>
    );
  }

}
