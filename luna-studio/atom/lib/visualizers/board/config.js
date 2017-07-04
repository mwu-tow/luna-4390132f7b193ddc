module.exports = function (type) {
    if (type.constructor === "List") return [{name: "table", path: "table/table.html"}];
    else if (type.constructor === "Text") return [{name: "text", path: "text/text.html"}];
};
