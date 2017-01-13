import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import WalletRecoveryPhraseShowDialog from '../app/components/wallet/WalletRecoveryPhraseShowDialog';

storiesOf('WalletRecovery', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('WalletRecoveryPhraseShowDialog', () => (
    <div>
      <WalletRecoveryPhraseShowDialog
        recoveryPhrase="cat dog duck mouse tree flower rainbow rain cloud sun sunset forest"
      />
    </div>
  ));

