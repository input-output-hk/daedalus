import React from 'react';
import { TransactionMetadataView } from '../../../../source/renderer/app/components/wallet/transactions/metadata/TransactionMetadataView';
import { EXAMPLE_METADATA } from '../../_support/utils';

export default {
  title: 'Wallets / Transactions',
};

export const Metadata = () => (
  <TransactionMetadataView data={EXAMPLE_METADATA} />
);
