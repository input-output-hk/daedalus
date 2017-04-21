// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './MnemonicWord.scss';

@observer
export default class MnemonicWord extends Component {

  props: {
    word: string,
    index: number,
    isActive: boolean,
    onClick: Function,
  };

  render() {
    const { word, index, isActive, onClick } = this.props;
    const componentClassNames = classnames([
      styles.component,
      isActive ? styles.active : styles.inactive
    ]);
    return (
      <button
        className={componentClassNames}
        onClick={() => onClick({ word, index })}
      >
        {word}
      </button>
    );
  }

}
