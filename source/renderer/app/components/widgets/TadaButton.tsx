import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TadaButton.scss' or its corr... Remove this comment to see the full error message
import styles from './TadaButton.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/tada-green... Remove this comment to see the full error message
import tadaIcon from '../../assets/images/tada-green-ic.inline.svg';

type Props = {
  onClick: (...args: Array<any>) => any;
  iconClass?: string;
  shouldAnimate: boolean;
};
export default class TadaButton extends Component<Props> {
  render() {
    const { onClick, iconClass, shouldAnimate } = this.props;
    const componentClasses = classNames([
      styles.component,
      shouldAnimate ? styles.animate : null,
      iconClass,
    ]);
    return (
      <button className={componentClasses} onClick={onClick}>
        <SVGInline className={styles.icon} svg={tadaIcon} />
      </button>
    );
  }
}
