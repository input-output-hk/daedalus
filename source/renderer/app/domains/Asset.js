// @flow
import { pick } from 'lodash';
import { observable, action } from 'mobx';
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
    Object.assign(this, data);
  }

  @action update(other: Asset) {
    Object.assign(
      this,
      pick(other, ['id', 'policyId', 'assetName', 'fingerprint', 'metadata'])
    );
  }
}
