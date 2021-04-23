// @flow
import { pick } from 'lodash';
import { observable, action } from 'mobx';
import type { AssetMetadata } from '../api/assets/types';

export type AssetProps = {
  policyId: string,
  assetName: string,
  fingerprint: string,
  metadata?: ?AssetMetadata,
  decimals: number,
};

export default class Asset {
  @observable policyId: string = '';
  @observable assetName: string = '';
  @observable fingerprint: string = '';
  @observable metadata: ?AssetMetadata;
  @observable decimals: number;

  constructor(data: AssetProps) {
    Object.assign(this, data);
  }

  @action update(other: $Shape<AssetProps>) {
    Object.assign(
      this,
      pick(other, [
        'policyId',
        'assetName',
        'fingerprint',
        'metadata',
        'decimals',
      ])
    );
  }
}
