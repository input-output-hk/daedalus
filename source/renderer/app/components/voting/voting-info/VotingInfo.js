// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import BorderedBox from '../../widgets/BorderedBox';
import type { Locale } from '../../../../../common/types/locales.types';
import styles from './VotingInfo.scss';

import CurrentFund from './CurrentFund';
import Headline from './Headline';
import AppStore from './AppStore';

type Props = {
  currentLocale: Locale,
  currentDateFormat: string,
  currentTimeFormat: string,
  isRegistrationEnded: boolean,
  onRegisterToVoteClick: Function,
  onExternalLinkClick: Function,
};

@observer
export default class VotingInfo extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      currentLocale,
      currentDateFormat,
      currentTimeFormat,
      onRegisterToVoteClick,
      onExternalLinkClick,
    } = this.props;

    return (
      <div className={styles.component}>
        <BorderedBox>
          <Headline onExternalLinkClick={onExternalLinkClick} />
          <hr className={styles.separator} />
          <div className={styles.bottomContent}>
            <div className={styles.leftContent}>
              <CurrentFund
                currentLocale={currentLocale}
                currentDateFormat={currentDateFormat}
                currentTimeFormat={currentTimeFormat}
                onExternalLinkClick={onRegisterToVoteClick}
              />
              <div className={styles.appStoreSpacing}>
                <AppStore
                  onAppleStoreLinkClick={onExternalLinkClick}
                  onAndroidStoreLinkClick={onExternalLinkClick}
                />
              </div>
            </div>
            <div className={styles.rightContent}>
              <div
                style={{
                  width: '549px',
                  height: '348px',
                  background: 'rgba(68, 91, 124, 0.1)',
                }}
              />
            </div>
          </div>
        </BorderedBox>
      </div>
    );
  }
}
