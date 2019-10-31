// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import spinnerIconBig from '../../assets/images/spinner-dark-big.inline.svg';
import spinnerIconSmall from '../../assets/images/spinner-dark.inline.svg';
import styles from './LoadingSpinner.scss';

type Props = {
  big?: boolean,
  medium?: boolean,
};

export default class LoadingSpinner extends Component<Props> {
  static defaultProps = {
    big: false,
    medium: false,
  };

  root: ?HTMLElement;

  render() {
    const { big, medium } = this.props;
    const icon = big ? spinnerIconBig : spinnerIconSmall;

    const componentClasses = classnames([
      styles.component,
      big ? styles.big : null,
      medium ? styles.medium : null,
      !big && !medium ? styles.small : null,
    ]);

    return (
      <div
        className={componentClasses}
        ref={div => {
          this.root = div;
        }}
      >
        <SVGInline svg={icon} className={styles.icon} />
      </div>
    );
  }
}
