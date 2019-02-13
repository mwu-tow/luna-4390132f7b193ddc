"use strict";

var getFontSize = function () {
    var el    = document.getElementsByTagName("atom-text-editor")[0];
    var style = window.getComputedStyle(el, null).getPropertyValue('font-size');
    return parseFloat(style);
};

module.exports = {
  getFontSize: getFontSize
};
