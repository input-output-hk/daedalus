import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import classNames from 'classnames';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/attenti... Remove this comment to see the full error message
import attentionIcon from '../../../assets/images/attention-big-light.inline.svg';
import { ALLOWED_TIME_DIFFERENCE } from '../../../config/timingConfig';
import humanizeDurationByLocale from '../../../utils/humanizeDurationByLocale';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SystemTimeError.scss' or its... Remove this comment to see the full error message
import styles from './SystemTimeError.scss';

const messages = defineMessages({
  overlayTitle: {
    id: 'systemTime.error.overlayTitle',
    defaultMessage: '!!!Unable to sync - incorrect time',
    description: 'Title of Sync error overlay',
  },
  overlayTextP1: {
    id: 'systemTime.error.overlayTextP1',
    defaultMessage:
      '!!!Attention, Daedalus is unable to sync with the blockchain because the time on your machine is different from the global time. Your time is off by 2 hours 12 minutes 54 seconds.',
    description: 'First paragraph of Sync error overlay',
  },
  overlayTextP2: {
    id: 'systemTime.error.overlayTextP2',
    defaultMessage:
      '!!!To synchronise the time and fix the issue, please read our {supportPortalLink} article.',
    description: 'Second paragraph of Sync error overlay',
  },
  ntpUnreachableTextP1: {
    id: 'systemTime.error.ntpUnreachableTextP1',
    defaultMessage:
      '!!!Attention, Daedalus is unable to check if the clock on your computer is synchronized with global time because NTP (Network Time Protocol) servers are unreachable, possibly due to firewalls on your network.',
    description: 'Text of Sync error overlay when NTP service is unreachable',
  },
  ntpUnreachableTextP2: {
    id: 'systemTime.error.ntpUnreachableTextP2',
    defaultMessage:
      '!!!If your computer clock is off by more than 15 seconds, Daedalus will be unable to connect to the network. If you have this issue, please read our Support Portal article to synchronize the time on your machine.',
    description: 'Text of Sync error overlay when NTP service is unreachable',
  },
  supportPortalLink: {
    id: 'systemTime.error.supportPortalLink',
    defaultMessage: '!!!Support Portal',
    description: '"Support Portal" link text',
  },
  supportPortalLinkUrl: {
    id: 'systemTime.error.supportPortalLinkUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360010230873',
    description:
      'Link to "Machine clock out of sync with Cardano network" support page',
  },
  onCheckTheTimeAgainLink: {
    id: 'systemTime.error.onCheckTheTimeAgainLink',
    defaultMessage: '!!!Check the time again',
    description: 'Text of Check the time again button',
  },
  onContinueWithoutClockSyncCheckLink: {
    id: 'systemTime.error.onContinueWithoutClockSyncCheckLink',
    defaultMessage: '!!!Continue without clock synchronization checks',
    description:
      'Text of "Continue without clock synchronization checks" button',
  },
});
type Props = {
  localTimeDifference: number | null | undefined;
  currentLocale: string;
  onExternalLinkClick: (...args: Array<any>) => any;
  onCheckTheTimeAgain: (...args: Array<any>) => any;
  onContinueWithoutClockSyncCheck: (...args: Array<any>) => any;
  isCheckingSystemTime: boolean;
};

@observer
class SystemTimeError extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      localTimeDifference,
      currentLocale,
      isCheckingSystemTime,
      onCheckTheTimeAgain,
      onContinueWithoutClockSyncCheck,
      onExternalLinkClick,
    } = this.props;
    const supportPortalLinkUrl = intl.formatMessage(
      messages.supportPortalLinkUrl
    );
    const supportPortalLink = (
      <Link
        className={styles.supportPortalLink}
        onClick={(event) => onExternalLinkClick(supportPortalLinkUrl, event)}
        label={intl.formatMessage(messages.supportPortalLink)}
        skin={LinkSkin}
      />
    );
    const isNTPServiceReachable = !!localTimeDifference;
    const allowedTimeDifferenceInSeconds = ALLOWED_TIME_DIFFERENCE / 1000000;
    const rawTimeOffset = (localTimeDifference || 0) / 1000;
    const timeOffset = humanizeDurationByLocale(rawTimeOffset, currentLocale, {
      delimiter: ' ',
      units: ['y', 'mo', 'w', 'd', 'h', 'm', 's', 'ms'],
      localeConfig: {
        'ja-JP': {
          spacer: '',
          delimiter: '',
          serialComma: false,
        },
      },
    });
    return (
      <div className={styles.component}>
        <SVGInline svg={attentionIcon} className={styles.icon} />

        {isNTPServiceReachable ? (
          <div>
            <p>
              <FormattedMessage
                {...messages.overlayTextP1}
                values={{
                  timeOffset: <em className="time-off">{timeOffset}</em>,
                }}
              />
            </p>

            <p>
              <FormattedMessage
                {...messages.overlayTextP2}
                values={{
                  supportPortalLink,
                }}
              />
            </p>

            <Link
              className={classNames([
                styles.checkLink,
                isCheckingSystemTime ? styles.disabled : null,
              ])}
              onClick={() => onCheckTheTimeAgain()}
              label={intl.formatMessage(messages.onCheckTheTimeAgainLink)}
              hasIconAfter={false}
              skin={LinkSkin}
            />
          </div>
        ) : (
          <div>
            <p>
              <FormattedMessage
                {...messages.ntpUnreachableTextP1}
                values={{
                  timeOffset,
                }}
              />
            </p>

            <p>
              <FormattedMessage
                {...messages.ntpUnreachableTextP2}
                values={{
                  supportPortalLink,
                  allowedTimeDifferenceInSeconds,
                }}
              />
            </p>

            <Link
              className={styles.checkLink}
              onClick={() => onContinueWithoutClockSyncCheck()}
              label={intl.formatMessage(
                messages.onContinueWithoutClockSyncCheckLink
              )}
              hasIconAfter={false}
              skin={LinkSkin}
            />
          </div>
        )}
      </div>
    );
  }
}

export default SystemTimeError;
