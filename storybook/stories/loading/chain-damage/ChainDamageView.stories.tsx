import React from 'react';
import { action } from '@storybook/addon-actions';
import { storiesOf } from '@storybook/react';
import ChainDamageView from '../../../../source/renderer/app/components/loading/chain-damage/ChainDamageView';
import RebuildConfirmView from '../../../../source/renderer/app/components/loading/chain-damage/RebuildConfirmView';
import type { ChainDamage } from '../../../../source/common/types/watchdog.types';
import StoryDecorator from '../../_support/StoryDecorator';
import LoadingOverlayStoryFrame from '../_support/LoadingOverlayStoryFrame';

const detectedAt = '2026-10-16T12:00:00.000Z';

const nodeRejects: ChainDamage = {
  kind: 'node_rejects_canonical_block',
  slot: 200361988,
  lastGoodSlot: null,
  detectedAt,
};

const ledgerPointMissing: ChainDamage = {
  kind: 'ledger_point_missing',
  slot: 200102375,
  lastGoodSlot: null,
  detectedAt,
};

const walletDiscontinuity: ChainDamage = {
  kind: 'wallet_chain_discontinuity',
  slot: 200102408,
  lastGoodSlot: 200080892,
  detectedAt,
};

const renderView = (
  damage: ChainDamage,
  initialView: 'choice' | 'confirm' = 'choice'
) => (
  <ChainDamageView
    damage={damage}
    initialView={initialView}
    onRebuild={action('onRebuild')}
    onDismiss={action('onDismiss')}
  />
);

storiesOf('Loading / Chain Damage', module)
  .addDecorator((story) => (
    <StoryDecorator>
      <LoadingOverlayStoryFrame>{story()}</LoadingOverlayStoryFrame>
    </StoryDecorator>
  ))
  .add('Node Rejects Canonical Block', () => renderView(nodeRejects))
  .add('Ledger Point Missing', () => renderView(ledgerPointMissing))
  .add('Wallet Chain Discontinuity', () => renderView(walletDiscontinuity))
  .add('Detection Without Slot', () =>
    renderView({ ...ledgerPointMissing, slot: null })
  )
  .add('Confirm Rebuild', () => renderView(ledgerPointMissing, 'confirm'))
  .add('Confirm Rebuild In Another Location', () => (
    <RebuildConfirmView
      location={'D:\\Cardano'}
      onRebuild={action('onRebuild')}
      onBack={action('onBack')}
    />
  ));
