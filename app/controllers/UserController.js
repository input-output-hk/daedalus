// @flow
import { action } from 'mobx';
import Profile from '../domain/Profile';
import BaseController from './BaseController';

export default class UserController extends BaseController {

  @action async updateField(field: string, value: string) {
    const { i18n } = this.state;
    const { profile } = this.stores.user.active;
    if (!profile) return;
    await this.api.updateProfileField({ field, value });
    switch (field) {
      case 'name': profile.name = value; break;
      case 'email': profile.email = value; break;
      case 'phoneNumber': profile.phoneNumber = value; break;
      case 'languageLocale':
        profile.languageLocale = value;
        i18n.locale = value;
        break;
      default:
    }
  }

}
