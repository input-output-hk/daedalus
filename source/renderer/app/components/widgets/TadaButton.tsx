import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './TadaButton.scss';
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
