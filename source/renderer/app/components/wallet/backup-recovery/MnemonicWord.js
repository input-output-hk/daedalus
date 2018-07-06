// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
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
    const handleClick = onClick.bind(null, { word, index });

    return (
      <Button
        className="flat"
        themeOverrides={styles}
        disabled={!isActive}
        label={word}
        onClick={handleClick}
        skin={ButtonSkin}
      />
    );
  }

}
