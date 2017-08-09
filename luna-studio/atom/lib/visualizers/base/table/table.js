(function () {

  var hasExactlyKeys = function (keys, obj) {
    return Object.keys(obj).length == keys.length && keys.every(k => obj.hasOwnProperty(k));
  };

  var isObjectMatrix = function (data) {
    var isList = Array.isArray(data) && data[0];
    if (!isList) return false;
    var firstKeys = Object.keys(data[0]);
    return data.every(obj => hasExactlyKeys(firstKeys, obj));
  }

  var genObjectMatrix = function (data) {
    var result = "<tr><th></th>";
    var keys   = Object.keys(data[0]);
    keys.forEach(function (key) {
      result += ("<th>" + key + "</th>");
    });
    result += "</tr>";
    data.forEach(function (row, ix) {
      result += ("<tr><th>" + ix + "</th>");
      keys.forEach(function (k) {
        result += ("<td>" + JSON.stringify(row[k]) + "</td>");
      });
      result += ("</tr>")
    });
    return ("<table>" + result + "</table>");
  }

  var isMatrix = function (data) {
    var isList = Array.isArray(data) && data[0];
    if (!isList) return false;
    var firstIsArray = Array.isArray(data[0]);
    if (!firstIsArray) return false;
    var firstLen = data[0].length;
    var eachHasProperLen = data.every(d => d.length == firstLen);
    return eachHasProperLen;
  }

  var genMatrix = function (data) {
    var result = "<tr><th></th>";
    data[0].forEach(function (elt, ix) {
      result += ("<th>" + ix + "</th>");
    });
    result += "</tr>";
    data.forEach(function (row, ix) {
      result += ("<tr><th>" + ix + "</th>");
      row.forEach(function (d) {
        result += ("<td>" + JSON.stringify(d) + "</td>");
      });
      result += ("</tr>")
    });
    return ("<table>" + result + "</table>");
  }

  var genGenericTable = function (data) {
    var result = "";
    data.forEach(function (point, ix) {
      result += ("<tr><th>" + ix + "</th><td>" + JSON.stringify(point) + "</td></tr>");
    });
    return ("<table>" + result + "</table>");
  }

  var genTable = function (data) {
    if (isMatrix(data)) {
      return genMatrix(data);
    } else if (isObjectMatrix(data)) {
      return genObjectMatrix(data);
    } else {
      return genGenericTable(data);
    }
  }

  var render = function (json) {
    var data  = JSON.parse(json);
    var table = genTable(data);
    document.body.innerHTML = table;
  };
  window.addEventListener("message", function (evt) {
    render(evt.data.data);
  });
}());
