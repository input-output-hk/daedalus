// @flow
import { action } from 'mobx';
import type { appState } from '../state/index';
import Profile from '../domain/Profile';
import api from '../api';

export default class WalletsController {

  state: appState;

  constructor(state: appState) {
    this.state = state;
  }

  @action async loadAccount() {
    const { account } = this.state;
    account.isLoading = true;
    try {
      const accountData = await api.loadAccount();
      const { profile } = accountData;
      account.userAccount.profile = new Profile(profile);
      account.isLoading = false;
    } catch (error) {
      account.errorLoading = 'Error loading account data'; // TODO: i18n
    }
  }


  @action async updateName(newNameData: { name: string }) {
    const { account } = this.state;
    account.isUpdatingName = true;
    try {
      await api.updateName(newNameData);
      // account.userAccount.profile.name = newNameData.name;
      account.isUpdatingName = false;
    } catch (error) {
      account.errorUpdatingName = 'Error updating name'; // TODO: i18n
    }
  }

}
