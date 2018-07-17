import { Component, OnInit } from '@angular/core';
import {ActivatedRoute} from '@angular/router';
import {CustomersService} from '../customers.service';
import {Customer} from '../customer';

@Component({
  selector: 'app-customer',
  templateUrl: './customer.component.html',
  styleUrls: ['./customer.component.css']
})
export class CustomerComponent implements OnInit {
  customer: Customer  = { id: null, first_name: '', last_name: '', age: null };

  constructor(
    private customersService: CustomersService,
    private activatedRoute: ActivatedRoute) { }

  ngOnInit() {
    let id = this.activatedRoute.snapshot.params['id'];
    this.customersService
      .getCustomerById(id)
      .subscribe((customer: Customer) => {
        this.customer = customer;
        console.log(customer);
      });
  }
}