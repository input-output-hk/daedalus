// @flow
import { action } from 'mobx';
import type { appState } from '../state/index';
import UserProfile from '../domain/UserProfile';
import api from '../api';

export default class AccountController {

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
      account.userAccount.profile = new UserProfile(profile);
      account.isLoading = false;
    } catch (error) {
      account.errorLoading = 'Error loading account data'; // TODO: i18n
    }
  }

  @action async updateField(field: string, value: string) {
    const { account } = this.state;
    const { profile } = account.userAccount;
    if (!profile) return;
    await api.updateProfileField({ field, value });
    switch (field) {
      case 'name': profile.name = value; break;
      case 'email': profile.email = value; break;
      case 'phoneNumber': profile.phoneNumber = value; break;
      default:
    }
  }

}
