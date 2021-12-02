// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { FormattedHTMLMessage } from 'react-intl';
import globalMessages from '../../i18n/global-messages';
import LoadingSpinner from '../widgets/LoadingSpinner';
import styles from './VotingUnavailable.scss';
import { formattedNumber } from '../../utils/formatters';

type Props = {
  syncPercentage: number,
};

@observer
export default class VotingUnavailable extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      syncPercentage,
      isVotingRegistrationAvailable,
      onExternalLinkClick,
    } = this.props;

    return (
      <div className={styles.component}>
        <LoadingSpinner big />
        <div className={styles.description}>
          <FormattedHTMLMessage
            {...globalMessages.featureUnavailableWhileSyncing}
            values={{
              syncPercentage: new BigNumber(syncPercentage).toFormat(2),
            }}
          />
        </div>
      </div>
    </div>
  );
};

export default observer(VotingUnavailable);
