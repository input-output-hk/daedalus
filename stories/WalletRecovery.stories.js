import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import WalletRecoveryPhraseShowDialog from '../app/components/wallet/backup-recovery/WalletRecoveryPhraseDialog';

const recoveryPhrase = [
  { word: 'cat' },
  { word: 'dog' },
  { word: 'duck' },
  { word: 'mouse' },
  { word: 'tree' },
  { word: 'flower' },
  { word: 'rainbow' },
  { word: 'rain' },
  { word: 'cloud' },
  { word: 'sunset' },
  { word: 'forest' }
];

const enteredPhrase = [
  { word: 'cat' },
  { word: 'dog' },
];

storiesOf('WalletRecovery', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('Backup', () => (
    <div>
      <WalletRecoveryPhraseShowDialog
        recoveryPhrase={recoveryPhrase}
        enteredPhrase={enteredPhrase}
        isEntering={false}
        isValid={false}
      />
    </div>
  ))

  .add('Verification - not valid', () => (
    <div>
      <WalletRecoveryPhraseShowDialog
        recoveryPhrase={recoveryPhrase}
        enteredPhrase={enteredPhrase}
        isEntering={true}
        isValid={false}
      />
    </div>
  ))

  .add('Verification - valid', () => (
    <div>
      <WalletRecoveryPhraseShowDialog
        recoveryPhrase={recoveryPhrase}
        enteredPhrase={enteredPhrase}
        isEntering={true}
        isValid={true}
      />
    </div>
  ));

