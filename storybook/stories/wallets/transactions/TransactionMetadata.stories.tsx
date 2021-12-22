import React from 'react';
import { storiesOf } from '@storybook/react';
import { TransactionMetadataView } from '../../../../source/renderer/app/components/wallet/transactions/metadata/TransactionMetadataView';
import { EXAMPLE_METADATA } from '../../_support/utils';

storiesOf('Wallets|Transactions', module) // ====== Stories ======
  .add('Metadata', () => <TransactionMetadataView data={EXAMPLE_METADATA} />);
