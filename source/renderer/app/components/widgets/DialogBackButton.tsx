import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/back-arrow... Remove this comment to see the full error message
import backArrow from '../../assets/images/back-arrow-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DialogBackButton.scss' or it... Remove this comment to see the full error message
import styles from './DialogBackButton.scss';

type Props = {
  onBack: (...args: Array<any>) => any;
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
