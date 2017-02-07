import { Component, OnInit } from '@angular/core';
import { Article } from '../models/articolo';
import { ArticlesService } from '../services/articles.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-articles-list',
  templateUrl: 'articles-list.component.html',
  styles: ['.error {color:red;}'],
  providers: [ArticlesService]
})
export class ArticlesListComponent implements OnInit {
  errorMessage: string;
  articoli: Article[];

  constructor(private articlesService: ArticlesService, private router: Router) { }

  ngOnInit() {
    this.getArticoli();
  }
  getArticoli() {
    this.articlesService.getArticoli()
      .subscribe(
      articoli => this.articoli = articoli,
      error => this.errorMessage = <any>error);
  }

  doNewArticle() {
    // use the Router service to imperatively navigate to another route
    this.router.navigate(['/article-edit', -1]);
  }

  doRemoveArticolo(index) {
    if (confirm('Are you sure?')) {
      this.articlesService.removeArticolo(this.articoli[index])
        .subscribe(resp => {
          this.getArticoli();
        });
    }
  }
}
