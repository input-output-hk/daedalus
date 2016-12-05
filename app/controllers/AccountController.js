// @flow
import { action } from 'mobx';
import Profile from '../domain/Profile';
import api from '../api';
import BaseController from './BaseController';

export default class AccountController extends BaseController {

  @action async loadAccount() {
    const { login, user } = this.state;
    login.isLoading = true;
    try {
      const accountData = await api.accounts.loadAccount();
      const { profile } = accountData;
      user.profile = new Profile(profile);
      login.isLoading = false;
    } catch (error) {
      login.errorLoading = 'Error loading account data'; // TODO: i18n
    }
  }

  @action async updateField(field: string, value: string) {
    const { user } = this.state;
    const { profile } = user;
    if (!profile) return;
    await api.accounts.updateProfileField({ field, value });
    switch (field) {
      case 'name': profile.name = value; break;
      case 'email': profile.email = value; break;
      case 'phoneNumber': profile.phoneNumber = value; break;
      default:
    }
  }

}
