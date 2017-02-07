/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { PrivateArea1Component } from './private-area1.component';

describe('PrivateArea1Component', () => {
  let component: PrivateArea1Component;
  let fixture: ComponentFixture<PrivateArea1Component>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ PrivateArea1Component ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(PrivateArea1Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
