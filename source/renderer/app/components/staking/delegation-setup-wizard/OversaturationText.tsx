import React from 'react';
import {
  defineMessages,
  FormattedHTMLMessage,
  injectIntl,
  intlShape,
} from 'react-intl';
import classnames from 'classnames';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import styles from './OversaturationText.scss';

const messages: Record<string, ReactIntlMessage> = defineMessages({
  oversaturationWarning: {
    id:
      'staking.delegationSetup.confirmation.step.dialog.oversaturationWarning',
    defaultMessage:
      '!!!The selected stake pool will become oversaturated by {oversaturationPercentage}%, which will reduce future rewards for all delegators to that pool.',
    description:
      'Warning shown if pool is going to be saturated if delegation happens',
  },
});
type Props = {
  intl: intlShape;
  oversaturationPercentage: string;
  centerText?: boolean;
};

function OversaturationTextComponent(props: Props) {
  const { oversaturationPercentage, centerText } = props;
  const oversaturationClasses = classnames([
    styles.component,
    centerText ? styles.centerText : null,
  ]);
  return (
    <p className={oversaturationClasses}>
      <FormattedHTMLMessage
        {...messages.oversaturationWarning}
        values={{
          oversaturationPercentage,
        }}
      />
    </p>
  );
}

export const OversaturationText = injectIntl(OversaturationTextComponent);
