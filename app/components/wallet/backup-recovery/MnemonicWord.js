// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './MnemonicWord.scss';

@observer
export default class MnemonicWord extends Component {

  static propTypes = {
    word: PropTypes.string.isRequired,
    isActive: PropTypes.bool.isRequired,
    onClick: PropTypes.func.isRequired
  };

  render() {
    const { word, isActive, onClick } = this.props;
    const componentClassNames = classnames([
      styles.component,
      isActive ? styles.active : styles.inactive
    ]);
    return (
      <button
        className={componentClassNames}
        onClick={() => onClick({ word })}
      >
        {word}
      </button>
    );
  }

}
