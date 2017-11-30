// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './MnemonicWord.scss';

type Props = {
  word: string,
  index: number,
  isActive: boolean,
  onClick: Function,
};

@observer
export default class MnemonicWord extends Component<Props> {

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
