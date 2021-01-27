// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import SVGInline from 'react-svg-inline';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './VotingInfo.scss';
import downloadAppIcon from '../../../assets/images/voting/download-app-ic.inline.svg';
import openAppIcon from '../../../assets/images/voting/open-app-ic.inline.svg';
import waitIcon from '../../../assets/images/voting/wait-ic.inline.svg';
import downloadAppStoreIcon from '../../../assets/images/voting/download-app-store-icon-ic.inline.svg';
import downloadPlayStoreIcon from '../../../assets/images/voting/download-play-store-icon-ic.inline.svg';

const messages = defineMessages({
  heading: {
    id: 'voting.info.heading',
    defaultMessage: '!!!Register to vote',
    description: 'Headline for voting registration steps',
  },
  description: {
    id: 'voting.info.description',
    defaultMessage:
      '!!!Before you begin, you must first complete the steps below:',
    description: 'Info description for voting registration steps',
  },
  stepTitle1: {
    id: 'voting.info.stepTitle1',
    defaultMessage: '!!!Download the Catalyst Voting App',
    description: 'Info step title 2 for voting registration steps',
  },
  stepTitle2: {
    id: 'voting.info.stepTitle2',
    defaultMessage:
      '!!!Open the Catalyst Voting App and click on the complete registration button.',
    description: 'Info step title 2 for voting registration steps',
  },
  stepTitle3: {
    id: 'voting.info.stepTitle3',
    defaultMessage:
      '!!!Ensure you have 5 minutes to complete the registration process, and an additional hour to receive a final confirmation.',
    description: 'Info step title 3 for voting registration steps',
  },
  buttonLabel: {
    id: 'voting.info.buttonLabel',
    defaultMessage:
      '!!!I confirm all steps have been completed. Continue with registration.',
    description: 'Button Label for voting registration steps',
  },
  androidAppButtonUrl: {
    id: 'voting.info.androidAppButtonUrl',
    defaultMessage:
      '!!!https://play.google.com/store/apps/details?id=io.iohk.vitvoting&hl=es_419&gl=US',
    description: '"androidAppButtonUrl" for the Catalyst voting app',
  },
});

type Props = {
  onRegisterToVoteClick: Function,
  onExternalLinkClick: Function,
};

@observer
export default class VotingInfo extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onRegisterToVoteClick, onExternalLinkClick } = this.props;
    const heading = intl.formatMessage(messages.heading);
    const description = intl.formatMessage(messages.description);
    const stepTitle1 = intl.formatMessage(messages.stepTitle1);
    const stepTitle2 = intl.formatMessage(messages.stepTitle2);
    const stepTitle3 = intl.formatMessage(messages.stepTitle3);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.heading}>{heading}</div>
          <div className={styles.description}>{description}</div>
          <div className={styles.stepsContainer}>
            <div className={styles.step}>
              <span className={styles.number}>1</span>
              <SVGInline svg={downloadAppIcon} className={styles.stepIcon} />
              <p>{stepTitle1}</p>
              <div className={styles.appStore}>
                <SVGInline
                  svg={downloadAppStoreIcon}
                  className={styles.stepAppStoreIcon}
                />
                <SVGInline
                  svg={downloadPlayStoreIcon}
                  className={styles.stepAppStoreIcon}
                  onClick={() => {
                    onExternalLinkClick(
                      intl.formatMessage(messages.androidAppButtonUrl)
                    );
                  }}
                />
              </div>
            </div>
            <div className={styles.step}>
              <span className={styles.number}>2</span>
              <SVGInline svg={openAppIcon} className={styles.stepIcon} />
              <p>{stepTitle2}</p>
            </div>
            <div className={styles.step}>
              <span className={styles.number}>3</span>
              <SVGInline svg={waitIcon} className={styles.stepIcon} />
              <p>{stepTitle3}</p>
            </div>
          </div>
          <Button
            onClick={onRegisterToVoteClick}
            skin={ButtonSkin}
            label={buttonLabel}
          />
        </BorderedBox>
      </div>
    );
  }
}
