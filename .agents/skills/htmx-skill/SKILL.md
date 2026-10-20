---
name: htmx-skill
description: Use when you need accurate htmx syntax or behavior — attributes, triggers, swap modifiers, events, request/response headers, the JS API, extensions, or a worked example. Indexes every page of the official htmx.org documentation with a description, so the right page can be fetched and read instead of recalled from memory.
---

# htmx Reference

Use the index below to open the most specific page for the user's question: read the linked
htmx.org page (WebFetch) instead of answering from memory — htmx's attribute semantics, inheritance
rules and swap modifiers are easy to misremember.

The descriptions below exist so you can pick the right page *before* fetching anything. If nothing in
the index matches, start from https://htmx.org/reference/ (the one-page reference of every attribute,
header, event and modifier).

## Reference Index

### General

- **Events**: Documentation page
  - Reference: https://htmx.org/events/

- **Javascript API**: This documentation describes the JavaScript API for htmx, including methods and properties for configuring the behavior of htmx, working with CSS classes, AJAX requests, event handling, and DOM manipulation. The API provides helper functions primarily intended for extension development and event management.
  - Reference: https://htmx.org/api/

### Attributes

- **hx-boost**: The hx-boost attribute in htmx enables progressive enhancement by converting standard HTML anchors and forms into AJAX requests, maintaining graceful fallback for users without JavaScript while providing modern dynamic page updates for those with JavaScript enabled.
  - Reference: https://htmx.org/attributes/hx-boost/

- **hx-confirm**: The hx-confirm attribute in htmx provides a way to add confirmation dialogs before executing requests, allowing you to protect users from accidental destructive actions. This documentation explains how to implement confirmation prompts and customize their behavior through event handling.
  - Reference: https://htmx.org/attributes/hx-confirm/

- **hx-delete**: The hx-delete attribute in htmx will cause an element to issue a DELETE request to the specified URL and swap the returned HTML into the DOM using a swap strategy.
  - Reference: https://htmx.org/attributes/hx-delete/

- **hx-disable**: The hx-disable attribute in htmx will disable htmx processing for a given element and all its children.
  - Reference: https://htmx.org/attributes/hx-disable/

- **hx-disabled-elt**: The hx-disabled-elt attribute in htmx allows you to specify elements that will have the `disabled` attribute added to them for the duration of the request.
  - Reference: https://htmx.org/attributes/hx-disabled-elt/

- **hx-disinherit**: The hx-disinherit attribute in htmx lets you control how child elements inherit attributes from their parents. This documentation explains how to selectively disable inheritance of specific htmx attributes or all attributes, allowing for more granular control over your web application's behavior.
  - Reference: https://htmx.org/attributes/hx-disinherit/

- **hx-encoding**: The hx-encoding attribute in htmx allows you to switch the request encoding from the usual `application/x-www-form-urlencoded` encoding to `multipart/form-data`, usually to support file uploads in an AJAX request.
  - Reference: https://htmx.org/attributes/hx-encoding/

- **hx-ext**: The hx-ext attribute in htmx enables one or more htmx extensions for an element and all its children. You can also use this attribute to ignore an extension that is enabled by a parent element.
  - Reference: https://htmx.org/attributes/hx-ext/

- **hx-get**: The hx-get attribute in htmx will cause an element to issue a GET request to the specified URL and swap the returned HTML into the DOM using a swap strategy.
  - Reference: https://htmx.org/attributes/hx-get/

- **hx-headers**: The hx-headers attribute in htmx allows you to add to the headers that will be submitted with an AJAX request.
  - Reference: https://htmx.org/attributes/hx-headers/

- **hx-history**: The hx-history attribute in htmx allows you to prevent sensitive page data from being stored in the browser's localStorage cache during history navigation, ensuring that the page state is retrieved from the server instead when navigating through history.
  - Reference: https://htmx.org/attributes/hx-history/

- **hx-history-elt**: The hx-history-elt attribute in htmx allows you to specify the element that will be used to snapshot and restore page state during navigation. In most cases we do not recommend using this element.
  - Reference: https://htmx.org/attributes/hx-history-elt/

- **hx-include**: The hx-include attribute in htmx allows you to include additional element values in an AJAX request.
  - Reference: https://htmx.org/attributes/hx-include/

- **hx-indicator**: The hx-indicator attribute in htmx allows you to specify the element that will have the `htmx-request` class added to it for the duration of the request. This can be used to show spinners or progress indicators while the request is in flight.
  - Reference: https://htmx.org/attributes/hx-indicator/

- **hx-inherit**: The hx-inherit attribute in htmx allows you to explicitly control attribute inheritance behavior between parent and child elements, providing fine-grained control over which htmx attributes are inherited when the default inheritance system is disabled through configuration.
  - Reference: https://htmx.org/attributes/hx-inherit/

- **hx-on**: The hx-on attributes in htmx allow you to write inline JavaScript event handlers directly on HTML elements, supporting both standard DOM events and htmx-specific events with improved locality of behavior.
  - Reference: https://htmx.org/attributes/hx-on/

- **hx-params**: The hx-params attribute in htmx allows you to filter the parameters that will be submitted with an AJAX request.
  - Reference: https://htmx.org/attributes/hx-params/

- **hx-patch**: The hx-patch attribute in htmx will cause an element to issue a PATCH request to the specified URL and swap the returned HTML into the DOM using a swap strategy.
  - Reference: https://htmx.org/attributes/hx-patch/

- **hx-post**: The hx-post attribute in htmx will cause an element to issue a POST request to the specified URL and swap the returned HTML into the DOM using a swap strategy.
  - Reference: https://htmx.org/attributes/hx-post/

- **hx-preserve**: The hx-preserve attribute in htmx allows you to keep an element unchanged during HTML replacement. Elements with hx-preserve set are preserved by `id` when htmx updates any ancestor element.
  - Reference: https://htmx.org/attributes/hx-preserve/

- **hx-prompt**: The hx-prompt attribute in htmx allows you to show a prompt before issuing a request. The value of the prompt will be included in the request in the `HX-Prompt` header.
  - Reference: https://htmx.org/attributes/hx-prompt/

- **hx-push-url**: The hx-push-url attribute in htmx allows you to push a URL into the browser location history. This creates a new history entry, allowing navigation with the browser's back and forward buttons.
  - Reference: https://htmx.org/attributes/hx-push-url/

- **hx-put**: The hx-put attribute in htmx will cause an element to issue a PUT request to the specified URL and swap the returned HTML into the DOM using a swap strategy.
  - Reference: https://htmx.org/attributes/hx-put/

- **hx-replace-url**: The hx-replace-url attribute in htmx allows you to replace the current URL of the browser location history.
  - Reference: https://htmx.org/attributes/hx-replace-url/

- **hx-request**: The hx-request attribute in htmx allows you to configure the request timeout, whether the request will send credentials, and whether the request will include headers.
  - Reference: https://htmx.org/attributes/hx-request/

- **hx-select**: The hx-select attribute in htmx allows you to select the content you want swapped from a response.
  - Reference: https://htmx.org/attributes/hx-select/

- **hx-select-oob**: The hx-select-oob attribute in htmx allows you to select content from a response to be swapped in via an out-of-band swap. The value of this attribute is comma separated list of elements to be swapped out of band.
  - Reference: https://htmx.org/attributes/hx-select-oob/

- **hx-swap**: The hx-swap attribute in htmx allows you to specify the 'swap strategy', or how the response will be swapped in relative to the target of an AJAX request. The default swap strategy is `innerHTML`.
  - Reference: https://htmx.org/attributes/hx-swap/

- **hx-swap-oob**: The hx-swap-oob attribute in htmx allows you to specify that some content in a response should be swapped into the DOM somewhere other than the target, that is 'out-of-band'. This allows you to piggyback updates to other elements on a response.
  - Reference: https://htmx.org/attributes/hx-swap-oob/

- **hx-sync**: The hx-sync attribute in htmx allows you to synchronize AJAX requests between multiple elements.
  - Reference: https://htmx.org/attributes/hx-sync/

- **hx-target**: The hx-target attribute in htmx allows you to target a different element for swapping than the one issuing the AJAX request.
  - Reference: https://htmx.org/attributes/hx-target/

- **hx-trigger**: The hx-trigger attribute in htmx allows you to specify what triggers an AJAX request. Supported triggers include standard DOM events, custom events, polling intervals, and event modifiers. The hx-trigger attribute also allows specifying event filtering, timing controls, event bubbling, and multiple trigger definitions for fine-grained control over when and how requests are initiated.
  - Reference: https://htmx.org/attributes/hx-trigger/

- **hx-validate**: The hx-validate attribute in htmx will cause an element to validate itself using the HTML5 Validation API before it submits a request.
  - Reference: https://htmx.org/attributes/hx-validate/

- **hx-vals**: The hx-vals attribute in htmx allows you to add to the parameters that will be submitted with an AJAX request.
  - Reference: https://htmx.org/attributes/hx-vals/

- **hx-vars**: The hx-vars attribute in htmx allows you to dynamically add to the parameters that will be submitted with an AJAX request. This attribute has been deprecated. We recommend you use the hx-vals attribute that provides the same functionality with safer defaults.
  - Reference: https://htmx.org/attributes/hx-vars/

### Examples

- **A Customized Confirmation UI**: Documentation page
  - Reference: https://htmx.org/examples/confirm/

- **Active Search**: Documentation page
  - Reference: https://htmx.org/examples/active-search/

- **Animations**: Documentation page
  - Reference: https://htmx.org/examples/animations/

- **Async Authentication**: Documentation page
  - Reference: https://htmx.org/examples/async-auth/

- **Bulk Update**: Documentation page
  - Reference: https://htmx.org/examples/bulk-update/

- **Cascading Selects**: Documentation page
  - Reference: https://htmx.org/examples/value-select/

- **Click to Edit**: Documentation page
  - Reference: https://htmx.org/examples/click-to-edit/

- **Click to Load**: Documentation page
  - Reference: https://htmx.org/examples/click-to-load/

- **Custom Modal Dialogs**: Documentation page
  - Reference: https://htmx.org/examples/modal-custom/

- **Delete Row**: Documentation page
  - Reference: https://htmx.org/examples/delete-row/

- **Dialogs**: Documentation page
  - Reference: https://htmx.org/examples/dialogs/

- **Edit Row**: Documentation page
  - Reference: https://htmx.org/examples/edit-row/

- **Experimental moveBefore() Support**: Documentation page
  - Reference: https://htmx.org/examples/move-before/details/

- **File Upload**: Documentation page
  - Reference: https://htmx.org/examples/file-upload/

- **Infinite Scroll**: Documentation page
  - Reference: https://htmx.org/examples/infinite-scroll/

- **Inline Validation**: Documentation page
  - Reference: https://htmx.org/examples/inline-validation/

- **Keyboard Shortcuts**: Documentation page
  - Reference: https://htmx.org/examples/keyboard-shortcuts/

- **Lazy Loading**: Documentation page
  - Reference: https://htmx.org/examples/lazy-load/

- **Modal Dialogs in Bootstrap**: Documentation page
  - Reference: https://htmx.org/examples/modal-bootstrap/

- **Modal Dialogs with UIKit**: Documentation page
  - Reference: https://htmx.org/examples/modal-uikit/

- **Preserving File Inputs after Form Errors**: Documentation page
  - Reference: https://htmx.org/examples/file-upload-input/

- **Progress Bar**: Documentation page
  - Reference: https://htmx.org/examples/progress-bar/

- **Reset user input**: Documentation page
  - Reference: https://htmx.org/examples/reset-user-input/

- **Sortable**: Documentation page
  - Reference: https://htmx.org/examples/sortable/

- **Tabs (Using HATEOAS)**: Documentation page
  - Reference: https://htmx.org/examples/tabs-hateoas/

- **Tabs (Using JavaScript)**: Documentation page
  - Reference: https://htmx.org/examples/tabs-javascript/

- **Updating Other Content**: Documentation page
  - Reference: https://htmx.org/examples/update-other-content/

- **Web Components**: Documentation page
  - Reference: https://htmx.org/examples/web-components/

### Extensions

- **Building htmx Extensions**: Documentation page
  - Reference: https://htmx.org/extensions/building/

- **htmx 1.x Compatibility Extension**: Documentation page
  - Reference: https://htmx.org/extensions/htmx-1-compat/

- **htmx Head Tag Support Extension**: Documentation page
  - Reference: https://htmx.org/extensions/head-support/

- **htmx Idiomorph Extension**: Documentation page
  - Reference: https://htmx.org/extensions/idiomorph/

- **htmx Preload Extension**: Documentation page
  - Reference: https://htmx.org/extensions/preload/

- **htmx Response Targets Extension**: Documentation page
  - Reference: https://htmx.org/extensions/response-targets/

- **htmx Server Sent Event (SSE) Extension**: Documentation page
  - Reference: https://htmx.org/extensions/sse/

- **htmx Web Socket extension**: Documentation page
  - Reference: https://htmx.org/extensions/ws/

### Headers

- **HX-Location Response Header**: Use the HX-Location response header in htmx to trigger a client-side redirection without reloading the whole page.
  - Reference: https://htmx.org/headers/hx-location/

- **HX-Push Response Header (Deprecated)**: The HX-Push response header in htmx is deprecated. Use HX-Push-Url instead.
  - Reference: https://htmx.org/headers/hx-push/

- **HX-Push-Url Response Header**: Use the HX-Push-Url response header in htmx to push a URL into the browser location history.
  - Reference: https://htmx.org/headers/hx-push-url/

- **HX-Redirect Response Header**: Use the HX-Redirect response header in htmx to trigger a client-side redirection that will perform a full page reload.
  - Reference: https://htmx.org/headers/hx-redirect/

- **HX-Replace-Url Response Header**: Use the HX-Replace-Url response header in htmx to replace the current URL in the browser location history without creating a new history entry.
  - Reference: https://htmx.org/headers/hx-replace-url/

- **HX-Trigger Response Headers**: Use the HX-Trigger family of response headers in htmx to trigger client-side actions from an htmx response.
  - Reference: https://htmx.org/headers/hx-trigger/


## Usage Notes

- Prefer attribute/event/header-specific pages over general guides.
- For API or configuration questions, also fetch https://htmx.org/api/.
- Fetch the referenced page to confirm details before answering if the description seems too brief.
