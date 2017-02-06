import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs/BehaviorSubject';


@Injectable()
export class AuthService {
  private userSubject: BehaviorSubject<string>;

  getSubject(): BehaviorSubject<string> {
    return this.userSubject;
  }

  getLoggedUser() {
    return localStorage['username'];
  }
  constructor() {
    let username = '';
    if (this.isLogged()) {
      username = this.getLoggedUser();
    }

    this.userSubject = new BehaviorSubject<string>(username);
    this.setLoggedUser(username);
  }

  setLoggedUser(username) {
    localStorage['username'] = username;
    this.userSubject.next(username);
  }

  isLogged() {
    return !!localStorage['username'];
  }

  logout() {
    this.setLoggedUser('');
    localStorage.clear();
  }

  login(username: string, password: string) {
    if (username === password) {
      this.setLoggedUser(username);
      return true;
    }
    return false;
  }
}
