import { observable, makeObservable } from 'mobx';

export default class Profile {
  name: string;
  email: string;
  phoneNumber: string;
  passwordHash: string;
  passwordUpdateDate: Date;
  languageLocale: string;

  constructor(data: {
    name: string;
    email: string;
    phoneNumber: string;
    passwordHash: string;
    passwordUpdateDate: Date;
    languageLocale: string;
  }) {
    makeObservable(this, {
      name: observable,
      email: observable,
      phoneNumber: observable,
      passwordHash: observable,
      passwordUpdateDate: observable,
      languageLocale: observable,
    });

    Object.assign(this, data);
  }
}
