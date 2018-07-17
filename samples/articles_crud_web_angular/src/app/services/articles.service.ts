import { Injectable } from '@angular/core';
import { Http, Response, Headers, RequestOptions } from '@angular/http';
import { Article } from '../models/articolo';
import { Observable } from 'rxjs';
import { environment } from '../../environments/environment';

@Injectable()
export class ArticlesService {
  private _articoliUrl = environment.URL; // 'http://localhost:8080/articles';  // URL to web api

  constructor(private http: Http) { }

  public getArticolo(id: number): Observable<Article> {
    return this.http
      .get(this._articoliUrl + '/' + id.toString())
      .map((res: Response) => <Article>res.json())
      .catch((err: any) => Observable.throw('Cannot find article'));
  }

  public getArticoli() {
    return this.http.get(this._articoliUrl)
      .map(res => <Article[]>res.json())
      .do(data => console.log(data)) // eyeball results in the console
      .catch(this.handleError);
  }
  private handleError(error: Response) {
    // in a real world app, we may send the error to some remote logging infrastructure
    // instead of just logging it to the console
    let err = null;
    try {
      err = error.json();
    } catch (ex) {
      err = { 'reasonstring': 'empty reasonstring', 'message': 'empty message' };
    }
    console.error(err);
    alert(err.message);
    return Observable.throw(error.text || 'Server error');
    //  return Observable.throw('Server error');
  }

  public addArticolo(articolo: Article): Observable<Article> {
    articolo.price = parseFloat(articolo.price.toString());
    let body = JSON.stringify(articolo);
    let headers = new Headers({ 'Content-Type': 'application/json' });
    let options = new RequestOptions({ headers: headers });

    return this.http.post(this._articoliUrl, body, options)
      .map(res => <Article>res.json())
      .catch(this.handleError);
  }

  public saveArticle(articolo: Article): Observable<Article> {
    articolo.price = parseFloat(articolo.price.toString());
    let body = JSON.stringify(articolo);
    let headers = new Headers({ 'Content-Type': 'application/json' });
    let options = new RequestOptions({ headers: headers });

    return this.http.put(
      this._articoliUrl + '/' + articolo.id.toString(), body, options)
      /*.map(res => <Articolo>res.json())*/
      .catch(this.handleError);
  }

  public removeArticolo(articolo: Article) {
    return this.http.delete(this._articoliUrl + '/' + articolo.id).catch(this.handleError);
  }
}
