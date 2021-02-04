// @flow
import { pick } from 'lodash';
import { observable, action } from 'mobx';
import type { AssetMetadata } from '../api/assets/types';

export type AssetProps = {
  id: string,
  policyId: string,
  assetName: string,
  metadata?: ?AssetMetadata,
};

export default class Asset {
  id: string = '';
  @observable policyId: string = '';
  @observable assetName: string = '';
  @observable metadata: ?AssetMetadata;

  constructor(data: AssetProps) {
    Object.assign(this, data);
  }

  @action update(other: Asset) {
    Object.assign(
      this,
      pick(other, ['id', 'policyId', 'assetName', 'metadata'])
    );
  }
}
