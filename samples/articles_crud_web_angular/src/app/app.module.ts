import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { PageNotFoundComponent } from './page-not-found/page-not-found.component';
import { ArticlesListComponent } from './articles-list/articles-list.component';
import { ArticlesService } from './services/articles.service';
import { ArticleEditComponent } from './article-edit/article-edit.component';
import { HomeComponent } from './home/home.component';
import { ProductListComponent } from './product-list/product-list.component';
import { ShoppingCartComponent } from './shopping-cart/shopping-cart.component';
import { ShoppingCartService } from './services/shopping-cart.service';
import { PrivateArea1Component } from './private-area1/private-area1.component';
import { PrivateArea2Component } from './private-area2/private-area2.component';
import { LoginComponent } from './login/login.component';
import { AuthGuard } from './guards/auth-guard';
import { AuthService } from './services/auth.service';
import { LoginLogoutComponent } from './login-logout/login-logout.component';

@NgModule({
  declarations: [
    AppComponent,
    PageNotFoundComponent,
    ArticlesListComponent,
    ArticleEditComponent,
    HomeComponent,
    ProductListComponent,
    ShoppingCartComponent,
    PrivateArea1Component,
    PrivateArea2Component,
    LoginComponent,
    LoginLogoutComponent
  ],
  imports: [
    BrowserModule,
    FormsModule,
    HttpModule,
    AppRoutingModule
  ],
  providers: [ArticlesService, ShoppingCartService, AuthGuard, AuthService],
  bootstrap: [AppComponent]
})
export class AppModule { }
