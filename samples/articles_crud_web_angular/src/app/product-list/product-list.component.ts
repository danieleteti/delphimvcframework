import { Component, OnInit } from '@angular/core';
import { ArticlesService } from '../services/articles.service';
import { Router } from '@angular/router';
import { Article } from '../models/articolo';
import { ShoppingCartService } from '../services/shopping-cart.service';

@Component({
  selector: 'app-product-list',
  templateUrl: './product-list.component.html',
  styleUrls: ['./product-list.component.css']
})
export class ProductListComponent implements OnInit {
  articles: Array<Article>;
  constructor(
    private articlesService: ArticlesService,
    private router: Router,
    private cartService: ShoppingCartService) { }

  ngOnInit() {
    this.getArticoli();
  }
  getArticoli() {
    this.articlesService.getArticoli()
      .subscribe(
      articoli => this.articles = articoli);
  }

  doAddProduct(index: number) {
    let article: Article = this.articles[index];
    this.cartService.addProduct(article.id, article.description, article.price);
  }

  doRemoveProduct(index: number) {
    let article: Article = this.articles[index];
    this.cartService.removeProduct(article.id);
  }

}


