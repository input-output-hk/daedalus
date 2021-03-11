// @flow
import { omit, pick } from 'lodash';
import { observable, action } from 'mobx';
import { IS_WALLET_ASSETS_FORMATTING_ENABLED } from '../config/walletsConfig';
import type { AssetMetadata } from '../api/assets/types';

export type AssetProps = {
  policyId: string,
  assetName: string,
  fingerprint: string,
  metadata?: ?AssetMetadata,
};

export default class Asset {
  @observable policyId: string = '';
  @observable assetName: string = '';
  @observable fingerprint: string = '';
  @observable metadata: ?AssetMetadata;

  constructor(data: AssetProps) {
    const metadata = !IS_WALLET_ASSETS_FORMATTING_ENABLED
      ? omit(data.metadata, 'unit')
      : data.metadata;
    Object.assign(this, data, { metadata });
  }

  @action update(other: Asset) {
    Object.assign(
      this,
      pick(other, ['policyId', 'assetName', 'fingerprint', 'metadata'])
    );
  }
}
