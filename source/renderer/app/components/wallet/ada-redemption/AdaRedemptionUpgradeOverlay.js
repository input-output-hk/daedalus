// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import attentionIcon from '../../../assets/images/attention-big-light.inline.svg';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
import externalLinkIcon from '../../../assets/images/link-ic.inline.svg';
import styles from './AdaRedemptionUpgradeOverlay.scss';

const messages = defineMessages({
  title: {
    id: 'wallet.redeem.upgradeOverlay.title',
    defaultMessage: '!!!Daedalus 0.14.AR for ada redemption',
    description: 'Title in Ada Redemption upgrade overlay',
  },
  explanation: {
    id: 'wallet.redeem.upgradeOverlay.explanation',
    defaultMessage:
      '!!!This is a special Daedalus version intended to be used for ada redemption. It will be available for a limited time. Daedalus 0.14.AR is based on a legacy Daedalus 0.14.0 version which was released with Cardano 1.6.',
    description: 'Explanation in Ada Redemption upgrade overlay',
  },
  emphasized: {
    id: 'wallet.redeem.upgradeOverlay.emphasized',
    defaultMessage:
      '!!!This version of Daedalus has been created specifically for users who purchased ada in the Japanese pre-sale and want to redeem their ada presale voucher.',
    description: 'Emphasized text in Ada Redemption upgrade overlay',
  },
  upgrade1: {
    id: 'wallet.redeem.upgradeOverlay.upgrade1',
    defaultMessage:
      '!!!If you did not purchase ada in the presale, or if you do not have unredeemed pre-sale vouchers, do not use this version. Instead, use the latest version available at',
    description: 'Suggestion to upgrade in Ada Redemption upgrade overlay',
  },
  upgrade2: {
    id: 'wallet.redeem.upgradeOverlay.upgrade2',
    defaultMessage:
      '!!!<a href="https://daedaluswallet.io">daedaluswallet.io</a>.',
    description: 'Suggestion to upgrade in Ada Redemption upgrade overlay',
  },
  button: {
    id: 'wallet.redeem.upgradeOverlay.button',
    defaultMessage: '!!!Read the instructions',
    description:
      'Read the instructions button in Ada Redemption upgrade overlay',
  },
  readInstructionsUrl: {
    id: 'wallet.redeem.upgradeOverlay.readInstructionsUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360036161714',
    description:
      'URL for "Read the instructions" button in Ada Redemption upgrade overlay',
  },
});

type Props = {
  onCloseOverlay: Function,
  onOpenExternalLink: Function,
  showOverlay: boolean,
};

@observer
export default class AdaRedemptionUpgradeOverlay extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

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
      onClick={() =>
        this.props.onOpenExternalLink(
          this.context.intl.formatMessage(messages.readInstructionsUrl)
        )
      }
    >
      {this.context.intl.formatMessage(messages.button)}
      <SVGInline svg={externalLinkIcon} />
    </button>
  );

  render() {
    const { intl } = this.context;
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
        <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
        <div
          className={styles.content}
          role="presentation"
          onClick={this.contentClickHandler.bind(this)}
        >
          <p>
            {intl.formatMessage(messages.explanation)}
            <br />
            <br />
            <strong>{intl.formatMessage(messages.emphasized)}</strong>
            <br />
            <br />
            {intl.formatMessage(messages.upgrade1)}{' '}
            <FormattedHTMLMessage {...messages.upgrade2} />
          </p>
        </div>
        {this.renderInstructionsBtn()}
      </div>
    );
  }
}
