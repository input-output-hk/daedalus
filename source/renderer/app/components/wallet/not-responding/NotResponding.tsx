import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/not-res... Remove this comment to see the full error message
import icon from '../../../assets/images/not-responding.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './NotResponding.scss' or its c... Remove this comment to see the full error message
import styles from './NotResponding.scss';

type Props = {
  walletName: string;
  onRestartNode: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
};
const messages = defineMessages({
  title: {
    id: 'wallet.notResponding.title',
    defaultMessage: '!!!The wallet is not responding.',
    description: 'Title on the NotResponding dialog.',
  },
  description: {
    id: 'wallet.notResponding.description',
    defaultMessage:
      '!!!The {walletName} wallet is not responding. This is caused by a known but rare issue, which is currently being fixed. Please restart the Cardano node by clicking the button below, which should resolve the issue. If the issue persists, or if it happens again, please submit a support request.',
    description: 'Description on the NotResponding dialog.',
  },
  restartNodeButtonLabel: {
    id: 'wallet.notResponding.restartNodeButtonLabel',
    defaultMessage: '!!!Restart Cardano Node',
    description: 'Restart Node Button Label on the NotResponding dialog.',
  },
  submitSupportRequestLabel: {
    id: 'wallet.notResponding.submitSupportRequestLabel',
    defaultMessage: '!!!Submit a support request',
    description: 'Submit Support Request Label on the NotResponding dialog',
  },
  submitSupportRequestUrl: {
    id: 'wallet.notResponding.submitSupportRequestUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description: 'Submit Support Request Url on the NotResponding dialog',
  },
});
export default class NotResponding extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { walletName, onRestartNode, onOpenExternalLink } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.content}>
          <SVGInline svg={icon} className={styles.icon} />
          <div className={styles.title}>
            {intl.formatMessage(messages.title)}
          </div>
          <div className={styles.description}>
            <FormattedHTMLMessage
              {...messages.description}
              values={{
                walletName,
              }}
            />
          </div>
          <Button
            className={styles.restartNodeButton}
            label={intl.formatMessage(messages.restartNodeButtonLabel)}
            onClick={onRestartNode}
            skin={ButtonSkin}
          />
          <Link
            className={styles.submitSupportLink}
            onClick={() =>
              onOpenExternalLink(
                intl.formatMessage(messages.submitSupportRequestUrl)
              )
            }
            label={intl.formatMessage(messages.submitSupportRequestLabel)}
            skin={LinkSkin}
          />
        </div>
      </div>
    );
  }
}
