module.exports = function (type) {
    if (type.constructor == "Geo" || (type.constructor == "Stream" && type.fields[0].constructor == "Geo")) return [{ path: "paper.html" }];
    else return [];
};
