/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { CustomersService } from './customers.service';

describe('Service: Customers', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [CustomersService]
    });
  });

  it('should ...', inject([CustomersService], (service: CustomersService) => {
    expect(service).toBeTruthy();
  }));
});
