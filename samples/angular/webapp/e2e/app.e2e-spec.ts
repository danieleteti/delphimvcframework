import { Myproj40Page } from './app.po';

describe('myproj40 App', function() {
  let page: Myproj40Page;

  beforeEach(() => {
    page = new Myproj40Page();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
