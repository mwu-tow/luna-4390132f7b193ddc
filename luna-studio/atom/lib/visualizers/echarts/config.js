module.exports = function (type) {
    var plotPattern = { constructor: ["Stream", "List"]
                      , fields:      [{constructor: ["Int", "Real"], fields: { any: true }}]
                      };
    var multiPlotPattern = { constructor: ["Stream", "List"]
                           , fields:      [ { constructor: ["List"]
                                            , fields: [{constructor: ["Int", "Real"], fields: { any: true }}] }
                                          ]
                           };
    var histogramPattern = { constructor: ["List"]
                           , fields:      [ { constructor: ["Pair"]
                                            , fields: [ { constructor: ["Text", "Int", "Real"]
                                                        , fields: { any: true } }
                                                      , { constructor: ["Int", "Real"]
                                                        , fields: { any: true } }
                                                      ]
                                            }
                                          ]
                           };
    var mapHistogramPattern = { constructor: ["Map"]
                              , fields:      [ { constructor: ["Text", "Int", "Real"]
                                               , fields: { any: true } }
                                             , { constructor: ["Int", "Real"]
                                               , fields: { any: true } }
                                             ]
                              };
    var geolocationPattern = { constructor: ["List"]
                             , fields:      [ { constructor: ["Pair"]
                                            , fields: [ { constructor: ["Real"]
                                                        , fields: { any: true } }
                                                      , { constructor: ["Real"]
                                                        , fields: { any: true } }
                                                      ]
                                            }
                                          ]
                             };
   var geolocationWithLabelsPattern = { constructor: ["List"]
                                      , fields:      [ { constructor: ["Pair"]
                                                       , fields: [ { constructor: ["Pair"]
                                                                   , fields: [ { constructor: ["Real"]
                                                                               , fields: { any: true } }
                                                                             , { constructor: ["Real"]
                                                                               , fields: { any: true } }
                                                                             ]
                                                                   }
                                                                 , { constructor: ["Text"]
                                                                   , fields: { any: true } }
                                                                 ]
                                                       }
                                                     ]
                                      };
    var plotVisualizer = (matches(type, plotPattern) || matches(type, multiPlotPattern)) ? [{name: "plot", path: "plot.html"}] : [];
    var histogramVisualizer = (matches(type, histogramPattern) || matches(type, mapHistogramPattern)) ? [{name: "histogram", path: "histogram.html"}] : [];
    var geolocationVisualizer = (matches(type, geolocationPattern) || matches(type, geolocationWithLabelsPattern)) ? [{name: "map", path: "map.html"}] : [];
    return [].concat(plotVisualizer, histogramVisualizer, geolocationVisualizer);
};



function matches(type, pattern) {
    if (pattern.any)
        return true
    else if (pattern.constructor.any || pattern.constructor.indexOf(type.constructor) != -1)
        if (pattern.fields.any)
            return true;
        else if (pattern.fields.length < type.fields.length)
            return false;
        else {
            for (var i = 0; i < type.fields.length; i++)
                if (!matches(type.fields[i], pattern.fields[i]))
                    return false;
            return true;
        }
    else
        return false;
}
