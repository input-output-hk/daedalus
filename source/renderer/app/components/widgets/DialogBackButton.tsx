// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import backArrow from '../../assets/images/back-arrow-ic.inline.svg';
import styles from './DialogBackButton.scss';

type Props = {
  onBack: Function,
};

export default class DialogBackButton extends Component<Props> {
  render() {
    const { onBack } = this.props;
    return (
      <button onClick={onBack} className={styles.component}>
        <SVGInline svg={backArrow} />
      </button>
    );
  }
}
