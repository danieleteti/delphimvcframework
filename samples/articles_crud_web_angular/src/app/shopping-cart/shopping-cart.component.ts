import { Component, OnInit } from '@angular/core';
import { ShoppingCartService, Product } from '../services/shopping-cart.service';

@Component({
  selector: 'app-shopping-cart',
  templateUrl: './shopping-cart.component.html',
  styleUrls: ['./shopping-cart.component.css']
})
export class ShoppingCartComponent implements OnInit {
  count: number = 0;
  total: number = 0;
  constructor(private cartService: ShoppingCartService) {
    cartService.getSubject().subscribe((value: Array<Product>) => {
      console.log(value);
      let c = 0;
      let total = 0;
      value.forEach((item: Product, index: number) => {
        c += item.quantity;
        console.log(item);
        total += +item.quantity * +item.price;

      });
      this.count = c;
      this.total = total;
    });
  }

  ngOnInit() {
  }

}
