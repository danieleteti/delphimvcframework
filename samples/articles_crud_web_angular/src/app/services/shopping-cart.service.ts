import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs/BehaviorSubject';

export interface Product {
  id: number;
  name: string;
  quantity: number;
  price: number;
}

@Injectable()
export class ShoppingCartService {
  private items: Array<Product> = [];
  private subject: BehaviorSubject<Array<Product>>;

  getSubject(): BehaviorSubject<Array<Product>> {
    return this.subject;
  }

  constructor() {
    this.subject = new BehaviorSubject<Array<Product>>([]);
  }

  addProduct(id: number, name: string, price: number) {
    let found = false;
    this.items.map((item: Product, index: number, items: Array<Product>) => {
      if (+item.id === +id) {
        item.quantity++;
        found = true;
        return false;
      }
    });
    if (!found) {
      this.items.push({ id: id, name: name, quantity: 1, price: price });
    }
    this.subject.next(this.items);
  }

  removeProduct(id: number) {
    let found = false;
    this.items.map((item: Product, index: number, items: Array<Product>) => {
      if (+item.id === +id) {
        item.quantity--;
      }
      if (item.quantity === 0) {
        items.splice(index, 1);
      }
    });
    // if (!found) {
    //   this.items.push({ id: id, name: name, quantity: 1 });
    // }
    this.subject.next(this.items);
  }


}
