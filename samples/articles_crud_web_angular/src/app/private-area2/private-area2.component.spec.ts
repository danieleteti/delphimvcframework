/* tslint:disable:no-unused-variable */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { DebugElement } from '@angular/core';

import { PrivateArea2Component } from './private-area2.component';

describe('PrivateArea2Component', () => {
  let component: PrivateArea2Component;
  let fixture: ComponentFixture<PrivateArea2Component>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ PrivateArea2Component ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(PrivateArea2Component);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
