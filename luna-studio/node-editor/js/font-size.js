var getFontSize = function () {
    el    = document.getElementsByTagName("atom-text-editor")[0];
    style = window.getComputedStyle(el, null).getPropertyValue('font-size');
    return parseFloat(style);
}

module.exports = {
  getFontSize: getFontSize
};
