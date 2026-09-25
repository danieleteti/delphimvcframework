var swaggerUiTheme = (function() {
  var storageKey = 'swagger-ui-theme';
  var defaultTheme = 'dark';

  function isTheme(value) {
    return value === 'dark' || value === 'light';
  }

  function readSavedTheme() {
    try {
      return window.localStorage.getItem(storageKey);
    } catch (e) {
      return null;
    }
  }

  function saveTheme(theme) {
    try {
      window.localStorage.setItem(storageKey, theme);
    } catch (e) {
      // the theme is not remembered when the browser blocks the storage
    }
  }

  function initialTheme() {
    var requested = new URLSearchParams(window.location.search).get('theme');
    if (isTheme(requested)) {
      return requested;
    }
    var saved = readSavedTheme();
    return isTheme(saved) ? saved : defaultTheme;
  }

  function currentTheme() {
    return document.documentElement.classList.contains('dark-mode') ? 'dark' : 'light';
  }

  function attachToggle(theme, attempts) {
    var button = document.querySelector('.dark-mode-toggle button');
    if (!button) {
      if (attempts > 0) {
        setTimeout(function() { attachToggle(theme, attempts - 1); }, 50);
      }
      return;
    }

    // the toggle button of Swagger UI keeps its own state, so the page class follows it before switching
    var pressed = button.getAttribute('aria-pressed') === 'true';
    document.documentElement.classList.toggle('dark-mode', pressed);
    if (pressed !== (theme === 'dark')) {
      button.click();
    }

    button.addEventListener('click', function() {
      setTimeout(function() { saveTheme(currentTheme()); }, 0);
    });
  }

  var theme = initialTheme();
  document.documentElement.classList.toggle('dark-mode', theme === 'dark');

  return {
    attach: function() { attachToggle(theme, 200); }
  };
})();

window.onload = function() {
  //<editor-fold desc="Changeable Configuration Block">

  // the following lines will be replaced by docker/configurator, when it runs in a docker-container
  window.ui = SwaggerUIBundle({
    url: "./openapi.json",
    validatorUrl: null,
    dom_id: '#swagger-ui',
    deepLinking: true,
    presets: [
      SwaggerUIBundle.presets.apis,
      SwaggerUIStandalonePreset
    ],
    plugins: [
      SwaggerUIBundle.plugins.DownloadUrl
    ],
    layout: "StandaloneLayout"
  });

  //</editor-fold>

  swaggerUiTheme.attach();
};
