import { RouterSamplePage } from './app.po';

describe('router-sample App', function() {
  let page: RouterSamplePage;

  beforeEach(() => {
    page = new RouterSamplePage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
