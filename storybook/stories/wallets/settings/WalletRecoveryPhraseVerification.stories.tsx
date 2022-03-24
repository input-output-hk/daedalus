import React from 'react';
import moment from 'moment';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import {
  withKnobs,
  select,
  boolean,
  object,
  optionsKnob as options,
} from '@storybook/addon-knobs';
import {
  LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../../../../source/renderer/app/config/cryptoConfig';
import { RECOVERY_PHRASE_VERIFICATION_TIMES as times } from '../../../../source/renderer/app/config/walletRecoveryPhraseVerificationConfig';
// Helpers
import StoryDecorator from '../../_support/StoryDecorator';
// Screens
import WalletRecoveryPhraseVerificationWidget from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseVerificationWidget';

storiesOf('Wallets|Settings', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  )) // ====== Stories ======
  .add(
    'Recovery Prase Verification - Widget',
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale }: { locale: string; }... Remove this comment to see the full error message
    ({ locale }: { locale: string }) => {
      const groupId = 'Recovery Phrase Verification';
      const wordCount = options(
        'Word count',
        {
          [WALLET_RECOVERY_PHRASE_WORD_COUNT]: `${WALLET_RECOVERY_PHRASE_WORD_COUNT}`,
          [LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT]: `${LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT}`,
        },
        `${WALLET_RECOVERY_PHRASE_WORD_COUNT}`,
        {
          display: 'inline-radio',
        },
        groupId
      );
      const veriticationTimeOptions = {
        '1 month ago': moment().subtract(30, 'days'),
        '2 months': moment().subtract(30 * 2, 'days'),
        '5 months ago': moment().subtract(30 * 5, 'days'),
        '6+ months ago': moment().subtract(times.warning + 1, 'days'),
        '1 year ago': moment().subtract(times.notification + 1, 'days'),
      };
      const creationTimeOptions = {
        '1 month ago': moment().subtract(30, 'days'),
        '2 months': moment().subtract(60, 'days'),
        '3-5 months': moment().subtract(times.okFewMonths + 1, 'days'),
        '5 months ago': moment().subtract(times.okFewWeeks + 1, 'days'),
        '1 week left for 6 months': moment().subtract(
          times.okFewDays + 1,
          'days'
        ),
        '6+ months ago': moment().subtract(times.warning + 1, 'days'),
        '1 year ago': moment().subtract(times.notification + 1, 'days'),
      };
      const wasAlreadyVerified = boolean('Already verified?', false, groupId);
      const creationDate = !wasAlreadyVerified
        ? select(
            'Wallet creation date',
            // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ '1 month ago': moment.Moment; ... Remove this comment to see the full error message
            creationTimeOptions,
            creationTimeOptions['1 month ago'],
            groupId
          )
        : creationTimeOptions['1 month ago'];
      const recoveryPhraseVerificationDate = wasAlreadyVerified
        ? select(
            'Last verification date',
            // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ '1 month ago': moment.Moment; ... Remove this comment to see the full error message
            veriticationTimeOptions,
            veriticationTimeOptions['1 month ago'],
            groupId
          )
        : null;
      const containerStyle = object('Container Style', {
        padding: 20,
      });
      return (
        <div style={containerStyle} className="WalletSettings_component">
          <WalletRecoveryPhraseVerificationWidget
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            creationDate={new Date(creationDate)}
            locale={locale}
            onVerify={action('onVerify')}
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            recoveryPhraseVerificationDate={recoveryPhraseVerificationDate}
            wordCount={parseInt(wordCount, 10)}
            isLegacy={boolean('isLegacy', true)}
          />
        </div>
      );
    }
  );
