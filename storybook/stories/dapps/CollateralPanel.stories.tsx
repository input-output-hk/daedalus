import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import CollateralPanel from '../../../source/renderer/app/components/dapp/collateral/CollateralPanel';
import type {
  CollateralPreference,
  CollateralState,
} from '../../../source/common/types/collateral.types';
import StoryDecorator from '../_support/StoryDecorator';

const states: readonly CollateralState[] = [
  'checking',
  'ready',
  'not-ready',
  'preparing',
  'in-use',
  'will-be-spent',
  'charged',
  'stale',
];
const preference = (state: CollateralState): CollateralPreference => ({
  schemaVersion: 1,
  walletId: 'ab'.repeat(20),
  networkGenesis: 'cd'.repeat(32),
  targetLovelace: '5000000',
  preferredInputs:
    state === 'checking' || state === 'not-ready' || state === 'preparing'
      ? []
      : [{ transactionId: 'ef'.repeat(32), index: 0 }],
  generation: 1,
  state,
});

storiesOf('dApps / Preferred collateral', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('all states', () => (
    <div style={{ maxWidth: 720, padding: 24 }}>
      {states.map((state) => (
        <CollateralPanel
          key={state}
          preference={preference(state)}
          corrupt={false}
          busy={false}
          failed={false}
          onPrepare={action(`prepare-${state}`)}
          onCancelPreparation={action(`cancel-${state}`)}
          onClear={action(`clear-${state}`)}
          onRepair={action(`repair-${state}`)}
        />
      ))}
    </div>
  ));
