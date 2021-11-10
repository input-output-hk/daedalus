// @flow

import { observable, action } from 'mobx';
import { Feature } from '../../utils/mobx-features/feature';

export class DiscreetMode extends Feature {
  @observable isDiscreetMode: boolean = false;

  @action toggleDiscreetMode = () => {
    this.isDiscreetMode = !this.isDiscreetMode;
  };
}
