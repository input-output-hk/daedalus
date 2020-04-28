// @flow
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

// Helpers
import StoryDecorator from '../../_support/StoryDecorator';

// Screens
import WalletRecoveryPhraseVerificationWidget from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseVerificationWidget';

const now = moment();

storiesOf('Wallets|Settings', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  // ====== Stories ======

  .add('Recovery Prase Verification - Widget', () => {
    const groupId = 'Recovery Phrase Verification';
    const wordCount = options(
      'Word count',
      {
        [WALLET_RECOVERY_PHRASE_WORD_COUNT]: `${WALLET_RECOVERY_PHRASE_WORD_COUNT}`,
        [LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT]: `${LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT}`,
      },
      `${WALLET_RECOVERY_PHRASE_WORD_COUNT}`,
      { display: 'inline-radio' },
      groupId
    );
    const timeOptions = {
      Now: now,
      '1 day ago': moment().subtract(1, 'days'),
      '7 days ago': moment().subtract(7, 'days'),
      '1 month ago': moment().subtract(1, 'months'),
      '5 months and 3 weeks ago': moment()
        .subtract(5, 'months')
        .subtract(3, 'weeks'),
      '6 months ago': moment().subtract(6, 'months'),
      '8 months ago': moment().subtract(8, 'months'),
      '1 year ago': moment().subtract(1, 'years'),
    };
    const wasAlreadyChecked = boolean('Already checked?', false, groupId);
    const creationDate = !wasAlreadyChecked
      ? select('Wallet creation date', timeOptions, timeOptions.Now, groupId)
      : timeOptions.Now;
    const recoveryPhraseVerificationDate = wasAlreadyChecked
      ? select('Last verification date', timeOptions, timeOptions.Now, groupId)
      : null;
    const containerStyle = object('Container Style', { padding: 20 });
    return (
      <div style={containerStyle}>
        <WalletRecoveryPhraseVerificationWidget
          creationDate={new Date(creationDate)}
          onVerify={action('onVerify')}
          recoveryPhraseVerificationDate={recoveryPhraseVerificationDate}
          wordCount={parseInt(wordCount, 10)}
        />
      </div>
    );
  });
