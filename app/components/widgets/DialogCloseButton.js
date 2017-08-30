import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import closeCross from '../../assets/images/close-cross.inline.svg';
import styles from './DialogCloseButton.scss';

export default class DialogCloseButton extends Component {

  props: {
    onClose: Function,
    icon?: string,
  };

  render() {
    const { onClose, icon } = this.props;
    return (
      <button onClick={onClose} className={styles.component}>
        <SvgInline svg={icon || closeCross} />
      </button>
    );
  }
}
