import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import closeCross from '../../assets/images/close-cross.inline.svg';
import styles from './DialogCloseButton.scss';

type Props = {
  onClose: Function,
  icon?: string,
};

export default class DialogCloseButton extends Component<Props> {

  render() {
    const { onClose, icon } = this.props;
    return (
      <button onClick={onClose} className={styles.component}>
        <SvgInline svg={icon || closeCross} />
      </button>
    );
  }
}
