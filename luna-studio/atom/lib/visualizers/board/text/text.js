(function () {
  var render = function (json) {
    var data = JSON.parse(json);
    document.body.innerHTML = "<p>" + data + "</p>";
  };
  window.addEventListener("message", function (evt) {
    render(evt.data.data);
  });
}());
