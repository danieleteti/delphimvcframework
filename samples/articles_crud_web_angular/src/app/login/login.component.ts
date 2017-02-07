import { Component, OnInit } from '@angular/core';
import { AuthService } from '../services/auth.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {
  message: string = '';
  user: { username: string, password: string } = { username: '', password: '' };
  constructor(private authService: AuthService, private router: Router) { }

  ngOnInit() {
  }

  doLogin() {
    this.message = '';
    if (this.authService.login(this.user.username, this.user.password)) {
      this.router.navigate(['/admin', 'private1']);
    } else {
      this.message = 'Invalid login';
    }
  }

}
