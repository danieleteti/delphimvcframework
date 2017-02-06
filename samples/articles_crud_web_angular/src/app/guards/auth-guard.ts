import { Injectable } from '@angular/core';
import { CanActivate, Router } from '@angular/router';
import { AuthService } from '../services/auth.service';

@Injectable()
export class AuthGuard implements CanActivate {
  constructor(private router: Router, private authService: AuthService) { }

  canActivate() {
    console.log('AuthGuard#canActivate called');
    return this.checkLogin();
  }

  checkLogin(): boolean {
    if (this.authService.isLogged()) {
      return true;
    }

    // Navigate to the login page
    this.router.navigate(['/login']);
    return false;
  }
}
