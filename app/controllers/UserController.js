// @flow
import { action } from 'mobx';
import Profile from '../domain/Profile';
import BaseController from './BaseController';

export default class UserController extends BaseController {

  @action async login(loginCredentials: {
    email: string,
    passwordHash: string
  }) {
    const { login } = this.state;
    login.isLoggingIn = true;
    login.isLoggedIn = false;
    try {
      const isLoginSuccessful = await this.api.login(loginCredentials);
      if (isLoginSuccessful) {
        login.isLoggedIn = true;
      }
      login.isLoggingIn = false;
    } catch (error) {
      login.errorLoggingIn = 'Error logging in'; // TODO: i18n
    }
  }

  @action async loadUser() {
    const { login, user } = this.state;
    login.isLoading = true;
    try {
      const userData = await this.api.getUser();
      user.id = userData.id;
      user.profile = new Profile(userData.profile);
      login.isLoading = false;
    } catch (error) {
      login.errorLoading = 'Error loading account data'; // TODO: i18n
    }
  }

  @action async updateField(field: string, value: string) {
    const { user } = this.state;
    const { profile } = user;
    if (!profile) return;
    await this.api.updateProfileField({ field, value });
    switch (field) {
      case 'name': profile.name = value; break;
      case 'email': profile.email = value; break;
      case 'phoneNumber': profile.phoneNumber = value; break;
      default:
    }
  }

}
