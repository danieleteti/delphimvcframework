import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router, Params } from '@angular/router';
import { ArticlesService } from '../services/articles.service';
import { Article } from '../models/articolo';

@Component({
  selector: 'app-article-edit',
  templateUrl: './article-edit.component.html',
  styleUrls: ['./article-edit.component.css']
})
export class ArticleEditComponent implements OnInit {
  private insertMode: boolean;
  public article: Article = new Article();
  constructor(private route: ActivatedRoute, private router: Router, private articlesService: ArticlesService) { }

  ngOnInit() {
    this.route.params.subscribe((data: Params) => {
      console.log(data['id']);
      this.insertMode = +data['id'] === -1;

      if (!this.insertMode) {
        this.articlesService.getArticolo(+data['id']).subscribe((articolo) => {
          this.article = articolo;
        });
      }
    });
  }

  doSaveArticle() {
    if (this.insertMode) {
      this.articlesService.addArticolo(this.article)
        .do(articolo => console.log(articolo))
        .subscribe((x) => this.router.navigate(['/articles-list']));
    } else {
      this.articlesService.saveArticle(this.article)
        .do(articolo => console.log(articolo))
        .subscribe((x) => this.router.navigate(['/articles-list']));
    }
  }

  doCancel() {
    this.router.navigate(['/articles-list']);
  }
}
