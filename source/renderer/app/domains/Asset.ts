// @flow
import { pick } from 'lodash';
import { observable, action, computed } from 'mobx';
import type { Asset as AssetProps, AssetMetadata } from '../api/assets/types';
import { hexToString } from '../utils/strings.js';

export default class Asset {
  @observable policyId: string = '';
  @observable assetName: string = '';
  @observable uniqueId: string = '';
  @observable fingerprint: string = '';
  @observable metadata: ?AssetMetadata;
  @observable decimals: ?number;
  @observable recommendedDecimals: ?number;
  @computed get assetNameASCII() {
    return hexToString(this.assetName || '');
  }

  constructor(props: AssetProps) {
    const { uniqueId } = props;
    Object.assign(this, props, { uniqueId });
  }

  @action update(props: $Shape<AssetProps>) {
    const { uniqueId } = props;
    Object.assign(
      this,
      pick(props, [
        'policyId',
        'assetName',
        'fingerprint',
        'metadata',
        'decimals',
        'recommendedDecimals',
      ]),
      { uniqueId }
    );
  }
}
