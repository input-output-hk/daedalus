// @flow
import React from 'react';
import {
  defineMessages,
  FormattedHTMLMessage,
  injectIntl,
  intlShape,
} from 'react-intl';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import styles from './DelegationStepsConfirmationDialog.scss';

const messages: { [string]: ReactIntlMessage } = defineMessages({
  oversaturationWarning: {
    id:
      'staking.delegationSetup.confirmation.step.dialog.oversaturationWarning',
    defaultMessage:
      '!!!The selected stake pool will become oversaturated, which will reduce future rewards for all delegators to that pool.',
    description:
      'Warning shown if pool is going to be saturated if delegation happens',
  },
});

type Props = {
  intl: intlShape,
};

const OversaturationText = (props: Props) => (
  <p className={styles.description}>
    <FormattedHTMLMessage
      {...messages.oversaturationWarning}
      values={{
        oversaturationPercentage: '140%',
      }}
    />
  </p>
);

export default injectIntl(OversaturationText);
