import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { ArticlesListComponent } from './articles-list/articles-list.component';
import { PageNotFoundComponent } from './page-not-found/page-not-found.component';
import { ArticleEditComponent } from './article-edit/article-edit.component';
import { HomeComponent } from './home/home.component';
import { ShoppingCartComponent } from './shopping-cart/shopping-cart.component';
import { ProductListComponent } from './product-list/product-list.component';
import { PrivateArea1Component } from './private-area1/private-area1.component';
import { PrivateArea2Component } from './private-area2/private-area2.component';
import { AuthGuard } from './guards/auth-guard';
import { LoginComponent } from './login/login.component';

// The router selects the route with a first match wins strategy.
// Wildcard routes are the least specific routes in the route configuration.
// Be sure it is the last route in the configuration.

const routes: Routes = [
  { path: 'home', component: HomeComponent },
  { path: 'articles-list', component: ArticlesListComponent },
  { path: 'article-edit/:id', component: ArticleEditComponent },
  // { path: 'shopping', component: ProductListComponent /*data: { title: 'Heroes List'}*/ },
  {
    path: '',
    redirectTo: '/home',
    pathMatch: 'full'
  },
  {
    path: 'admin', canActivate: [AuthGuard],
    children: [
      { path: 'private1', component: PrivateArea1Component },
      { path: 'private2', component: PrivateArea2Component }
    ]
  },
  { path: 'login', component: LoginComponent },
  { path: '**', component: PageNotFoundComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule],
  providers: []
})
export class AppRoutingModule { }
