// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/ButtonSkin';
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
      'flat',
      styles.component,
      isActive ? styles.active : styles.inactive
    ]);
    return (
      <Button
        className={componentClassNames}
        label={word}
        onClick={() => onClick({ word, index })}
        skin={<SimpleButtonSkin />}
      />
    );
  }

}
