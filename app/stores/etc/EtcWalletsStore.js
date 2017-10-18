// @flow
import { observable } from 'mobx';
import WalletStore from '../WalletStore';
import Request from '.././lib/LocalizedRequest';

export default class EtcWalletsStore extends WalletStore {

  @observable walletsRequest: Request<any> = new Request(this.api.etc.getWallets);

}
