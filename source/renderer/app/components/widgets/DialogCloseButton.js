// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import closeCross from '../../assets/images/close-cross.inline.svg';
import styles from './DialogCloseButton.scss';

type Props = {
  onClose?: Function,
  icon?: string,
  disabled?: boolean,
  className?: string,
};

export default class DialogCloseButton extends Component<Props> {
  render() {
    const { onClose, icon, disabled, className } = this.props;
    const buttonClass = !className ? '' : className;
    return (
      <button
        onClick={onClose != null ? onClose : () => {}}
        className={
          !disabled
            ? `${styles.component} ${buttonClass}`
            : `${styles.disabled} ${buttonClass}`
        }
        tabIndex={-1}
      >
        <SVGInline svg={icon || closeCross} />
      </button>
    );
  }
}
