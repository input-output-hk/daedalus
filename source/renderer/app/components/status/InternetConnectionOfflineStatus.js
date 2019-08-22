// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import disconnectedIcon from '../../assets/images/disconnected.inline.svg';
import styles from './InternetConnectionOfflineStatus.scss';

const messages = defineMessages({
  disconnectedTitle: {
    id: 'daedalus.internetConnectionStatus.dialog.disconnectedTitle',
    defaultMessage: '!!!No Internet connection',
    description: 'No internet connection title',
  },
  disconnectedDescription: {
    id: 'daedalus.internetConnectionStatus.dialog.disconnectedDescription',
    defaultMessage:
      '!!!Daedalus cannot operate without an Internet connection.',
    description: 'No internet connection description',
  },
  checkAgainLabel: {
    id: 'daedalus.internetConnectionStatus.dialog.checkAgainLabel',
    defaultMessage: '!!!Check again',
    description: 'Check again button label',
  },
});

type Props = {
  checkAgain: Function,
};

export default class InternetConnectionOfflineStatus extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { checkAgain } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.disconnectedIconContainer}>
          <SVGInline
            svg={disconnectedIcon}
            className={styles.disconnectedIcon}
          />
        </div>
        <div className={styles.disconnectedTitle}>
          <span>{intl.formatMessage(messages.disconnectedTitle)}</span>
        </div>
        <div className={styles.disconnectedDescription}>
          <span>{intl.formatMessage(messages.disconnectedDescription)}</span>
        </div>
        <div className={styles.disconnectedAction}>
          <Button
            className={styles.checkAgainButton}
            label={intl.formatMessage(messages.checkAgainLabel)}
            onClick={checkAgain}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}
