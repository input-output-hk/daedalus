// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import QRCode from 'qrcode.react';
import { Button } from 'react-polymorph/lib/components/Button';
import { Link } from 'react-polymorph/lib/components/Link';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import SVGInline from 'react-svg-inline';
import BorderedBox from '../widgets/BorderedBox';
import {
  VOTING_REGISTRATION_END_DATE,
  VOTING_REGISTRATION_CAST_START_DATE,
  VOTING_REGISTRATION_CAST_END_DATE,
  VOTING_REGISTRATION_NEW_START_DATE,
} from '../../config/votingConfig';
import { formattedDateTime } from '../../utils/formatters';
import type { Locale } from '../../../../common/types/locales.types';
import styles from './VotingInfo.scss';
import downloadAppStoreIcon from '../../assets/images/voting/download-app-store-icon-ic.inline.svg';
import downloadPlayStoreIcon from '../../assets/images/voting/download-play-store-icon-ic.inline.svg';

const messages = defineMessages({
  heading: {
    id: 'voting.info.heading',
    defaultMessage: '!!!Register to vote on Fund3',
    description: 'Headline for voting registration steps',
  },
  headingForEndedRegistration: {
    id: 'voting.info.headingForEndedRegistration',
    defaultMessage: '!!!Fund3 Voting Registration Ended',
    description: 'Headline for ended voting registration',
  },
  descriptionForEndedRegistration: {
    id: 'voting.info.descriptionForEndedRegistration',
    defaultMessage:
      '!!!<p>Voting registration for Project Catalyst Fund3 has been completed. The voting snapshot took place on <b>{snapshotDate}</b>.</p><p>If you have registered to vote on Fund3, you can cast your vote using the Catalyst Voting mobile app between <b>{castStartDate}</b> and <b>{castEndDate}</b>.</p><p>Fund4 registration will start on <b>{newRegistrationStartDate}</b>.</p>',
    description: 'Description for ended voting registration',
  },
  stepTitle1: {
    id: 'voting.info.stepTitle1',
    defaultMessage:
      '!!!Decide which innovative ideas for Cardano will receive funding.',
    description: 'Info step title 2 for voting registration steps',
  },
  stepTitle2: {
    id: 'voting.info.stepTitle2',
    defaultMessage:
      '!!!$70.000 worth of ada rewards will be distributed between ada holders who register their vote.',
    description: 'Info step title 2 for voting registration steps',
  },
  learnMorePreviousLabel: {
    id: 'voting.info.learnMorePreviousLabel',
    defaultMessage: '!!!Please read the',
    description: 'Learn more link previous label',
  },
  learnMoreNextLabel: {
    id: 'voting.info.learnMoreNextLabel',
    defaultMessage: '!!!for more information.',
    description: 'Learn more link next label',
  },
  learnMoreLinkLabel: {
    id: 'voting.info.learnMoreLinkLabel',
    defaultMessage: '!!!Learn more',
    description: 'Learn more link label for registration steps',
  },
  learnMoreLinkLabelForEndedRegistration: {
    id: 'voting.info.learnMoreLinkLabelForEndedRegistration',
    defaultMessage: '!!!Fund3 FAQs',
    description: 'Learn more link label for ended registration',
  },
  learnMoreLinkUrl: {
    id: 'voting.info.learnMoreLinkUrl',
    defaultMessage: '!!!https://cardano.ideascale.com/a/index',
    description: 'Learn more link url for registration steps',
  },
  learnMoreLinkUrlForEndedRegistration: {
    id: 'voting.info.learnMoreLinkUrlForEndedRegistration',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/900004448046',
    description: 'Learn more link url for ended registration',
  },
  bottomContentTitle: {
    id: 'voting.info.bottomContentTitle',
    defaultMessage: '!!!Download the Catalyst Voting app on your smartphone',
    description: 'bottomContentTitle for voting registration steps',
  },
  bottomContentDescription: {
    id: 'voting.info.bottomContentDescription',
    defaultMessage:
      '!!!To register to vote for Catalyst Fund3 you first need to download the <b>Catalyst Voting</b> app on your Android or iOS smartphone.',
    description: 'bottomContentDescription for voting registration steps',
  },
  bottomContentDescriptionForEndedRegistration: {
    id: 'voting.info.bottomContentDescriptionForEndedRegistration',
    defaultMessage:
      '!!!To cast your vote on Project Catalyst Fund3 proposals, you need to download the <b>Catalyst Voting</b> app on your Android or iOS smartphone.',
    description: 'bottomContentDescription for ended registration',
  },
  checkboxLabel: {
    id: 'voting.info.checkboxLabel',
    defaultMessage: '!!!I have installed the Catalyst Voting app',
    description: 'checkboxLabel for voting registration steps',
  },
  buttonLabel: {
    id: 'voting.info.buttonLabel',
    defaultMessage: '!!!Register to vote',
    description: 'Button Label for voting registration steps',
  },
  androidAppButtonUrl: {
    id: 'voting.info.androidAppButtonUrl',
    defaultMessage:
      '!!!https://play.google.com/store/apps/details?id=io.iohk.vitvoting',
    description: '"androidAppButtonUrl" for the Catalyst voting app',
  },
  appleAppButtonUrl: {
    id: 'voting.info.appleAppButtonUrl',
    defaultMessage:
      '!!!https://apps.apple.com/in/app/catalyst-voting/id1517473397',
    description: '"appleAppButtonUrl" for the Catalyst voting app',
  },
});

type Props = {
  currentLocale: Locale,
  currentDateFormat: string,
  currentTimeFormat: string,
  isRegistrationEnded: boolean,
  onRegisterToVoteClick: Function,
  onExternalLinkClick: Function,
};

type State = {
  isAppInstalled: boolean,
};

@observer
export default class VotingInfo extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isAppInstalled: false,
  };

  handleIsAppInstalled = () => {
    this.setState((prevState) => ({
      isAppInstalled: !prevState.isAppInstalled,
    }));
  };

  render() {
    const { intl } = this.context;
    const {
      currentLocale,
      currentDateFormat,
      currentTimeFormat,
      isRegistrationEnded,
      onRegisterToVoteClick,
      onExternalLinkClick,
    } = this.props;
    const { isAppInstalled } = this.state;
    const headingMessage = isRegistrationEnded
      ? messages.headingForEndedRegistration
      : messages.heading;
    const heading = intl.formatMessage(headingMessage);
    const learnMoreLinkLabelMessage = isRegistrationEnded
      ? messages.learnMoreLinkLabelForEndedRegistration
      : messages.learnMoreLinkLabel;
    const learnMoreLinkLabel = intl.formatMessage(learnMoreLinkLabelMessage);
    const learnMoreLinkUrlMessage = isRegistrationEnded
      ? messages.learnMoreLinkUrlForEndedRegistration
      : messages.learnMoreLinkUrl;
    const learnMoreLinkUrl = intl.formatMessage(learnMoreLinkUrlMessage);
    const stepTitle1 = intl.formatMessage(messages.stepTitle1);
    const stepTitle2 = intl.formatMessage(messages.stepTitle2);
    const bottomContentTitle = intl.formatMessage(messages.bottomContentTitle);
    const bottomContentDescription = isRegistrationEnded
      ? messages.bottomContentDescriptionForEndedRegistration
      : messages.bottomContentDescription;
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const appleAppButtonUrl = intl.formatMessage(messages.appleAppButtonUrl);
    const androidAppButtonUrl = intl.formatMessage(
      messages.androidAppButtonUrl
    );
    const checkboxLabel = intl.formatMessage(messages.checkboxLabel);
    const snapshotDate = formattedDateTime(VOTING_REGISTRATION_END_DATE, {
      currentLocale,
      currentDateFormat,
      currentTimeFormat,
    });
    const castStartDate = formattedDateTime(
      VOTING_REGISTRATION_CAST_START_DATE,
      {
        currentLocale,
        currentDateFormat,
        currentTimeFormat,
      }
    );
    const castEndDate = formattedDateTime(VOTING_REGISTRATION_CAST_END_DATE, {
      currentLocale,
      currentDateFormat,
      currentTimeFormat,
    });
    const newRegistrationStartDate = formattedDateTime(
      VOTING_REGISTRATION_NEW_START_DATE,
      {
        currentLocale,
        currentDateFormat,
        currentTimeFormat,
      }
    );

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.heading}>{heading}</div>
          {isRegistrationEnded ? (
            <div className={styles.descriptionForEndedRegistration}>
              <FormattedHTMLMessage
                {...messages.descriptionForEndedRegistration}
                values={{
                  snapshotDate,
                  castStartDate,
                  castEndDate,
                  newRegistrationStartDate,
                }}
              />
              <p className={styles.learnMoreContainer}>
                {intl.formatMessage(messages.learnMorePreviousLabel)}
                <Link
                  label={learnMoreLinkLabel}
                  onClick={() => onExternalLinkClick(learnMoreLinkUrl)}
                  className={styles.learnMoreLink}
                />
                {intl.formatMessage(messages.learnMoreNextLabel)}
              </p>
            </div>
          ) : (
            <>
              <div className={styles.stepsContainer}>
                <div className={styles.step}>
                  <span className={styles.number}>1</span>
                  <p>{stepTitle1}</p>
                </div>
                <div className={styles.step}>
                  <span className={styles.number}>2</span>
                  <p>{stepTitle2}</p>
                </div>
              </div>
              <Link
                label={learnMoreLinkLabel}
                onClick={() => onExternalLinkClick(learnMoreLinkUrl)}
              />
            </>
          )}
          <hr className={styles.separator} />
          <div className={styles.bottomContent}>
            <div className={styles.leftContent}>
              <p>
                <b>{bottomContentTitle}</b>
              </p>
              <FormattedHTMLMessage {...bottomContentDescription} />
              {!isRegistrationEnded && (
                <Checkbox
                  label={checkboxLabel}
                  onChange={this.handleIsAppInstalled}
                  className={styles.checkbox}
                  checked={isAppInstalled}
                />
              )}
            </div>
            <div className={styles.rightContent}>
              <div className={styles.appStore}>
                <div className={styles.appStoreItem}>
                  <button
                    className={styles.appStoreButton}
                    onClick={() => {
                      onExternalLinkClick(appleAppButtonUrl);
                    }}
                  >
                    <SVGInline svg={downloadAppStoreIcon} />
                  </button>
                  <div className={styles.qrCode}>
                    <QRCode
                      value={appleAppButtonUrl}
                      size={75}
                      renderAs="svg"
                    />
                  </div>
                </div>
                <div className={styles.appStoreItem}>
                  <button
                    className={styles.appStoreButton}
                    onClick={() => {
                      onExternalLinkClick(androidAppButtonUrl);
                    }}
                  >
                    <SVGInline svg={downloadPlayStoreIcon} />
                  </button>
                  <div className={styles.qrCode}>
                    <QRCode
                      value={androidAppButtonUrl}
                      size={75}
                      renderAs="svg"
                    />
                  </div>
                </div>
              </div>
            </div>
          </div>
          {!isRegistrationEnded && (
            <Button
              onClick={onRegisterToVoteClick}
              label={buttonLabel}
              disabled={!isAppInstalled}
            />
          )}
        </BorderedBox>
      </div>
    );
  }
}
