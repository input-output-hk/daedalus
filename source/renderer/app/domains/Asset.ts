import { pick } from 'lodash';
import { observable, action, computed, makeObservable } from 'mobx';
import type { Asset as AssetProps, AssetMetadata } from '../api/assets/types';
import { hexToString } from '../utils/strings';

export default class Asset {
  policyId = '';
  assetName = '';
  uniqueId = '';
  fingerprint = '';
  metadata: AssetMetadata | null | undefined;
  decimals: number | null | undefined;
  recommendedDecimals: number | null | undefined;

  get assetNameASCII() {
    return hexToString(this.assetName || '');
  }

  constructor(props: AssetProps) {
    makeObservable(this, {
      policyId: observable,
      assetName: observable,
      uniqueId: observable,
      fingerprint: observable,
      metadata: observable,
      decimals: observable,
      recommendedDecimals: observable,
      assetNameASCII: computed,
      update: action,
    });

    const { uniqueId } = props;
    Object.assign(this, props, {
      uniqueId,
    });
  }

  update(props: Partial<AssetProps>) {
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
      {
        uniqueId,
      }
    );
  }
}
