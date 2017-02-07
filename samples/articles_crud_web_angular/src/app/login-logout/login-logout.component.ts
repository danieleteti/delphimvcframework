import { Component, OnInit } from '@angular/core';
import { AuthService } from '../services/auth.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-login-logout',
  templateUrl: './login-logout.component.html',
  styleUrls: ['./login-logout.component.css']
})
export class LoginLogoutComponent implements OnInit {
  action: string = '';
  constructor(private authService: AuthService, private router: Router) { }

  ngOnInit() {
    this.authService.getSubject().subscribe((username) => {
      if (this.authService.isLogged()) {
        this.action = 'logout';
      } else {
        this.action = 'login';
      }
    });
  }

  doLoginLogout() {
    if (this.authService.isLogged()) {
      this.authService.logout();
      this.router.navigate(['/home']);
    } else {
      this.router.navigate(['/login']);
    }
  }
}
