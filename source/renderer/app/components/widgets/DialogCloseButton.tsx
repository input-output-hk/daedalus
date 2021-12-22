import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/close-cros... Remove this comment to see the full error message
import closeCross from '../../assets/images/close-cross.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DialogCloseButton.scss' or i... Remove this comment to see the full error message
import styles from './DialogCloseButton.scss';

type Props = {
  onClose?: (...args: Array<any>) => any;
  icon?: string;
  disabled?: boolean;
  className?: string;
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
