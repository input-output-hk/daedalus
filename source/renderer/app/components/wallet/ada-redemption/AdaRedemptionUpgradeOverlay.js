// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import attentionIcon from '../../../assets/images/attention-big-light.inline.svg';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
import externalLinkIcon from '../../../assets/images/link-ic.inline.svg';
import styles from './AdaRedemptionUpgradeOverlay.scss';

type Props = {
  onCloseOverlay: Function,
  onOpenExternalLink: Function,
  showOverlay: boolean,
};

@observer
export default class AdaRedemptionUpgradeOverlay extends Component<Props> {
  contentClickHandler(event: SyntheticMouseEvent<HTMLElement>) {
    const linkUrl = get(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    }
  }

  renderInstructionsBtn = () => (
    <button
      className={styles.actionBtn}
      onClick={() => this.props.onOpenExternalLink('https://daedaluswallet.io')}
    >
      Read the instructions
      <SVGInline svg={externalLinkIcon} />
    </button>
  );

  render() {
    const { onCloseOverlay, showOverlay } = this.props;

    if (!showOverlay) {
      return null;
    }

    return (
      <div className={styles.component}>
        <DialogCloseButton
          className={styles.closeButton}
          icon={closeCrossThin}
          onClose={onCloseOverlay}
        />
        <SVGInline className={styles.attentionIcon} svg={attentionIcon} />
        <h1 className={styles.title}>Daedalus 0.14.0 for ada redemption</h1>
        <div
          className={styles.content}
          role="presentation"
          onClick={this.contentClickHandler.bind(this)}
        >
          <p>
            This is a special Daedalus version intended to be used for ada
            redemption. It will be available for a limited time. Daedalus
            0.14.AR is based on a legacy Daedalus 0.14.0 version which was
            released with Cardano 1.6.
            <br />
            <br />
            <strong>
              This version of Daedalus has been created specifically for users
              who purchased ada in the Japanese pre-sale and want to redeem
              their ada presale voucher.
            </strong>
            <br />
            <br />
            If you did not purchase ada in the presale, or if you do not have
            unredeemed pre-sale vouchers, do not use this version. Instead, use
            the latest version available at{' '}
            <a href="https://daedaluswallet.io">
              daedaluswallet.io
              <SVGInline svg={externalLinkIcon} />
            </a>
            .
          </p>
        </div>
        {this.renderInstructionsBtn()}
      </div>
    );
  }
}
