import { Component, OnInit } from '@angular/core';
import { AuthService } from './services/auth.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit {
  isLogged: boolean;
  constructor(private authService: AuthService) { }
  ngOnInit() {
    this.authService.getSubject().subscribe((username) => {
      this.isLogged = this.authService.isLogged();
    });
  }
}
