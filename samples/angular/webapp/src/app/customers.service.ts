import { Injectable } from '@angular/core';
import { Observable } from 'rxjs/Observable';
import { Http } from '@angular/http';
import 'rxjs/add/operator/map';
import {Customer} from './customer';

@Injectable()
export class CustomersService {
  BASEURL = 'http://localhost:8080';

  constructor(private http: Http) { }

  getCustomers(): Observable<Customer[]> {
    return this.http
      .get(this.BASEURL + '/api/customers')
      .map((response) => <Customer[]>response.json());
  }

  getCustomerById(id: Number): Observable<Customer> {
    return this.http
      .get(this.BASEURL + '/api/customers/' + id)
      .map((response) => <Customer>response.json());
  }
}