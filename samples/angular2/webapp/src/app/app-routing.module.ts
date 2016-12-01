import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import {HomeComponent} from './home/home.component';
import {CustomersComponent} from './customers/customers.component';
import {CustomerComponent} from './customer/customer.component';

const routes: Routes = [
  {
    path: 'home',
    component: HomeComponent
  },
  {
    path: 'customers',
    component: CustomersComponent
  },
  {
    path: 'customers/:id',
    component: CustomerComponent
  },
  {
    path: '**',
    redirectTo: 'home'
  }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule],
  providers: []
})
export class Myproj40RoutingModule { }
