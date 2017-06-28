(function () {
  var chart = null;
  var currentData = null;
  window.addEventListener("load", function () {
    chart  = echarts.init(document.getElementById("chart-container"));
    chart.setOption(defaultOptions);
  });
  window.addEventListener("resize", function () {
    if (chart) chart.resize();
  });

  function clearSeries() {
    options = chart.getOption();
    options.series = [];
    chart.setOption(options, { notMerge: true });
    return options;
  }

  function prepareBoxPlotData (rawData) {
      var boxData = [];
      var outliers = [];
      var axisData = [];

      function quantile(ascArr, p) {
        var H = (ascArr.length - 1) * p + 1,
          h = Math.floor(H),
          v = +ascArr[h - 1],
          e = H - h;
        return e ? v + e * (ascArr[h] - v) : v;
      };

      for (var i = 0; i < rawData.length; i++) {
          axisData.push(i + '');
          var ascList = rawData[i].sort();

          var Q1 = quantile(ascList, 0.25);
          var Q2 = quantile(ascList, 0.5);
          var Q3 = quantile(ascList, 0.75);
          var IQR = Q3 - Q1;

          var low = ascList[0];
          var high = ascList[ascList.length - 1];

          boxData.push([low, Q1, Q2, Q3, high]);

          for (var j = 0; j < ascList.length; j++) {
              var dataItem = ascList[j];
              if (dataItem < low || dataItem > high) {
                  var outlier = [i, dataItem];
                  outliers.push(outlier);
              }
          }
      }
      return {
          boxData: boxData,
          outliers: outliers,
          axisData: axisData
      };
  };

  function addLegendData (options) {
    options.legend[0].data = options.series.map(function(s) { return s.name; });
    return options;
  }

  function applyData (options, data) {
    if (!options.series) options.series = [];
    if (options.toolbox[0].feature.myBoxPlot.iconStatus.myBoxPlot == "emphasis") {
      boxData = prepareBoxPlotData(data);
      options.xAxis[0].data = boxData.axisData;
      options.series = [
          {
              name: 'boxplot',
              type: 'boxplot',
              data: boxData.boxData,
              tooltip: {
                  formatter: function (param) {
                      return [
                          'upper: ' + param.data[4],
                          'Q3: ' + param.data[3],
                          'median: ' + param.data[2],
                          'Q1: ' + param.data[1],
                          'lower: ' + param.data[0]
                      ].join('<br/>')
                  }
              }
          },
          {
              name: 'outlier',
              type: 'scatter',
              data: boxData.outliers
          }
      ];
    } else if (options.xAxis[0].type == "value") {
      data.forEach(function (d, i) {
        entryData = d.map(function (x, j) { return [j,x]; });
        if (options.series[i]) {
          options.series[i].data = entryData;
        } else {
          options.series[i] = { name: "Data" + i, type: "line", areaStyle: {empahsis: {}}, symbolSize: 1, symbol: "circle", data: entryData };
        }
      });
    } else if (options.xAxis[0].type == "category") {
      dataLength = Math.max.apply(null, data.map(function (s) { return s.length; }));
      options.xAxis[0].data = Array.apply(null, {length: dataLength}).map(Number.call, Number);
      data.forEach(function (entryData, i) {
        if (options.series[i]) {
          options.series[i].data = entryData;
        } else {
          options.series[i] = { name: "Data" + i, type: "line", areaStyle: {empahsis: {}}, symbolSize: 1, symbol: "circle", data: entryData };
        }
      });
    }
    return addLegendData(options);
  }

  var defaultOptions = {
    legend: [{
      data: [],
      formatter: function () {
        return '';
      },
      left: 35,
      top: 20,
      itemWidth: 15,
      itemHeight: 10,
      itemGap: 5
    }],
    toolbox: {
      show: true,
      right: 30,
      feature: {

        myLinePlot: {
          show: true,
          title: 'line',
          icon: 'path://M432.45,595.444c0,2.177-4.661,6.82-11.305,6.82c-6.475,0-11.306-4.567-11.306-6.82s4.852-6.812,11.306-6.812C427.841,588.632,432.452,593.191,432.45,595.444L432.45,595.444z M421.155,589.876c-3.009,0-5.448,2.495-5.448,5.572s2.439,5.572,5.448,5.572c3.01,0,5.449-2.495,5.449-5.572C426.604,592.371,424.165,589.876,421.155,589.876L421.155,589.876z M421.146,591.891c-1.916,0-3.47,1.589-3.47,3.549c0,1.959,1.554,3.548,3.47,3.548s3.469-1.589,3.469-3.548C424.614,593.479,423.062,591.891,421.146,591.891L421.146,591.891zM421.146,591.891',
          iconStatus: {myLinePlot: 'emphasis'},
          onclick: function (){
            options = chart.getOption();
            if (options.toolbox[0].feature.myBoxPlot.iconStatus.myBoxPlot == "emphasis") options = clearSeries();
            options.toolbox[0].feature.myLinePlot.iconStatus = {myLinePlot: 'emphasis'};
            options.toolbox[0].feature.myLinePlotWithFacilityField.iconStatus = {myLinePlotWithFacilityField: 'normal'};
            options.toolbox[0].feature.myBarPlot.iconStatus = {myBarPlot: 'normal'};
            options.toolbox[0].feature.myScatterPlot.iconStatus = {myScatterPlot: 'normal'};
            options.toolbox[0].feature.myBoxPlot.iconStatus = {myBoxPlot: 'normal'};
            options.xAxis[0].type = "value";
            options.xAxis[0].boundaryGap = undefined;
            options = applyData(options, currentData, true);
            options.series.map(function(s) {
              s.type = 'line';
              s.areaStyle = undefined;
              s.stack = undefined;
              s.symbolSize = 1;
              return s;
            });
            chart.setOption(options);
          }
        },

        myLinePlotWithFacilityField: {
          show: true,
          title: 'stack',
          icon: 'path://M432.45,595.444c0,2.177-4.661,6.82-11.305,6.82c-6.475,0-11.306-4.567-11.306-6.82s4.852-6.812,11.306-6.812C427.841,588.632,432.452,593.191,432.45,595.444L432.45,595.444z M421.155,589.876c-3.009,0-5.448,2.495-5.448,5.572s2.439,5.572,5.448,5.572c3.01,0,5.449-2.495,5.449-5.572C426.604,592.371,424.165,589.876,421.155,589.876L421.155,589.876z M421.146,591.891c-1.916,0-3.47,1.589-3.47,3.549c0,1.959,1.554,3.548,3.47,3.548s3.469-1.589,3.469-3.548C424.614,593.479,423.062,591.891,421.146,591.891L421.146,591.891zM421.146,591.891',
          iconStatus: {myLinePlotWithFacilityField: 'normal'},
          onclick: function (){
            options = chart.getOption();
            if (options.toolbox[0].feature.myBoxPlot.iconStatus.myBoxPlot == "emphasis") options = clearSeries();
            options.toolbox[0].feature.myLinePlot.iconStatus = {myLinePlot: 'normal'};
            options.toolbox[0].feature.myLinePlotWithFacilityField.iconStatus = {myLinePlotWithFacilityField: 'emphasis'};
            options.toolbox[0].feature.myBarPlot.iconStatus = {myBarPlot: 'normal'};
            options.toolbox[0].feature.myScatterPlot.iconStatus = {myScatterPlot: 'normal'};
            options.toolbox[0].feature.myBoxPlot.iconStatus = {myBoxPlot: 'normal'};
            options.xAxis[0].type = "category";
            options.xAxis[0].boundaryGap = false;
            options = applyData(options, currentData);
            options.series.map(function(s) {
              s.type = 'line';
              s.areaStyle = {normal: {}};
              s.stack = 'def';
              s.symbolSize = 1;
              return s;
            });
            chart.setOption(options);
          }
        },


        myBarPlot: {
          show: true,
          title: 'bar',
          icon: 'path://M432.45,595.444c0,2.177-4.661,6.82-11.305,6.82c-6.475,0-11.306-4.567-11.306-6.82s4.852-6.812,11.306-6.812C427.841,588.632,432.452,593.191,432.45,595.444L432.45,595.444z M421.155,589.876c-3.009,0-5.448,2.495-5.448,5.572s2.439,5.572,5.448,5.572c3.01,0,5.449-2.495,5.449-5.572C426.604,592.371,424.165,589.876,421.155,589.876L421.155,589.876z M421.146,591.891c-1.916,0-3.47,1.589-3.47,3.549c0,1.959,1.554,3.548,3.47,3.548s3.469-1.589,3.469-3.548C424.614,593.479,423.062,591.891,421.146,591.891L421.146,591.891zM421.146,591.891',
          iconStatus: {myBarPlot: 'normal'},
          onclick: function (){
            options = chart.getOption();
            if (options.toolbox[0].feature.myBoxPlot.iconStatus.myBoxPlot == "emphasis") options = clearSeries();
            options.toolbox[0].feature.myLinePlot.iconStatus = {myLinePlot: 'normal'};
            options.toolbox[0].feature.myLinePlotWithFacilityField.iconStatus = {myLinePlotWithFacilityField: 'normal'};
            options.toolbox[0].feature.myBarPlot.iconStatus = {myBarPlot: 'emphasis'};
            options.toolbox[0].feature.myScatterPlot.iconStatus = {myScatterPlot: 'normal'};
            options.toolbox[0].feature.myBoxPlot.iconStatus = {myBoxPlot: 'normal'};
            options.xAxis[0].type = "category";
            options.xAxis[0].boundaryGap = true;
            options = applyData(options, currentData);
            options.series.map(function(s) {
              s.type = 'bar';
              s.areaStyle = undefined;
              s.stack = undefined;
              s.symbolSize = 1;
              return s;
            });
            chart.setOption(options);
          }
        },

        myScatterPlot: {
          show: true,
          title: 'scatter',
          icon: 'path://M432.45,595.444c0,2.177-4.661,6.82-11.305,6.82c-6.475,0-11.306-4.567-11.306-6.82s4.852-6.812,11.306-6.812C427.841,588.632,432.452,593.191,432.45,595.444L432.45,595.444z M421.155,589.876c-3.009,0-5.448,2.495-5.448,5.572s2.439,5.572,5.448,5.572c3.01,0,5.449-2.495,5.449-5.572C426.604,592.371,424.165,589.876,421.155,589.876L421.155,589.876z M421.146,591.891c-1.916,0-3.47,1.589-3.47,3.549c0,1.959,1.554,3.548,3.47,3.548s3.469-1.589,3.469-3.548C424.614,593.479,423.062,591.891,421.146,591.891L421.146,591.891zM421.146,591.891',
          iconStatus: {myScatterPlot: 'normal'},
          onclick: function (){
            options = chart.getOption();
            if (options.toolbox[0].feature.myBoxPlot.iconStatus.myBoxPlot == "emphasis") options = clearSeries();
            options.toolbox[0].feature.myLinePlot.iconStatus = {myLinePlot: 'normal'};
            options.toolbox[0].feature.myLinePlotWithFacilityField.iconStatus = {myLinePlotWithFacilityField: 'normal'};
            options.toolbox[0].feature.myBarPlot.iconStatus = {myBarPlot: 'normal'};
            options.toolbox[0].feature.myScatterPlot.iconStatus = {myScatterPlot: 'emphasis'};
            options.toolbox[0].feature.myBoxPlot.iconStatus = {myBoxPlot: 'normal'};
            options.xAxis[0].type = "value";
            options.xAxis[0].boundaryGap = undefined;
            options = applyData(options, currentData);
            options.series.map(function(s) {
              s.type = 'scatter';
              s.areaStyle = undefined;
              s.stack = undefined;
              s.symbolSize = 3;
              return s;
            });
            chart.setOption(options);
          }
        },

        myBoxPlot: {
          show: true,
          title: 'box',
          icon: 'path://M432.45,595.444c0,2.177-4.661,6.82-11.305,6.82c-6.475,0-11.306-4.567-11.306-6.82s4.852-6.812,11.306-6.812C427.841,588.632,432.452,593.191,432.45,595.444L432.45,595.444z M421.155,589.876c-3.009,0-5.448,2.495-5.448,5.572s2.439,5.572,5.448,5.572c3.01,0,5.449-2.495,5.449-5.572C426.604,592.371,424.165,589.876,421.155,589.876L421.155,589.876z M421.146,591.891c-1.916,0-3.47,1.589-3.47,3.549c0,1.959,1.554,3.548,3.47,3.548s3.469-1.589,3.469-3.548C424.614,593.479,423.062,591.891,421.146,591.891L421.146,591.891zM421.146,591.891',
          iconStatus: {myBoxPlot: 'normal'},
          onclick: function (){
            options = clearSeries(chart.getOption());
            options.toolbox[0].feature.myLinePlot.iconStatus = {myLinePlot: 'normal'};
            options.toolbox[0].feature.myLinePlotWithFacilityField.iconStatus = {myLinePlotWithFacilityField: 'normal'};
            options.toolbox[0].feature.myBarPlot.iconStatus = {myBarPlot: 'normal'};
            options.toolbox[0].feature.myScatterPlot.iconStatus = {myScatterPlot: 'normal'};
            options.toolbox[0].feature.myBoxPlot.iconStatus = {myBoxPlot: 'emphasis'};
            options.xAxis[0].type = "category";
            options.xAxis[0].boundaryGap = true;
            chart.setOption(applyData(options, currentData));
          }
        },

        dataZoom: {
          show: true,
          title: {
            zoom: 'area zooming',
            back: 'restore area zooming'
          }
        },

        myResetView: {
          show: true,
          title: 'reset view',
          icon: 'path://M432.45,595.444c0,2.177-4.661,6.82-11.305,6.82c-6.475,0-11.306-4.567-11.306-6.82s4.852-6.812,11.306-6.812C427.841,588.632,432.452,593.191,432.45,595.444L432.45,595.444z M421.155,589.876c-3.009,0-5.448,2.495-5.448,5.572s2.439,5.572,5.448,5.572c3.01,0,5.449-2.495,5.449-5.572C426.604,592.371,424.165,589.876,421.155,589.876L421.155,589.876z M421.146,591.891c-1.916,0-3.47,1.589-3.47,3.549c0,1.959,1.554,3.548,3.47,3.548s3.469-1.589,3.469-3.548C424.614,593.479,423.062,591.891,421.146,591.891L421.146,591.891zM421.146,591.891',
          onclick: function (){
            options = chart.getOption();
            options.legend   = defaultOptions.legend;
            options.dataZoom = defaultOptions.dataZoom;
            chart.setOption(addLegendData(options), { notMerge: true });
          }
        }
      }
    },
    grid: {
      top: 75,
      left: 40,
      right: 40
    },

    tooltip : {
      trigger: 'item',
      axisPointer:{
           show: true,
           type : 'cross',
           lineStyle: {
               type : 'dashed',
               width : 1
           }
       },
       formatter : function (params) {
           return '(' + params.value[0] + ', ' + params.value[1] + ')';
       }
    },
    dataZoom: [{
      bottom: 25,
      id: "dataZoomX",
      type: "slider",
      xAxisIndex: [0],
      filterMode: "filter",
      dataBackground: "red",
    },
    {
      type: "inside"
    }],
    textStyle: {
      fontFamily: "monospace",
      fontSize: 10
    },
    xAxis: {
      position: "top",
      splitLine: {
        lineStyle: {
          color: "#333"
        }
      },
      axisPointer: {
        label: {show: false}
      },
      type: "value"
    },
    yAxis: {
      min: "dataMin",
      axisLabel: {
        textStyle: {
          fontSize: 8
        }
      },
      splitLine: {
        lineStyle: {
          color: "#333"
        }
      },
      axisPointer: {
        label: {show: false}
      },
      type: "value"
    },
    series: []
  };

  window.addEventListener("message", function (evt) {
    mayOptions = chart.getOption();
    newOptions = mayOptions ? mayOptions : defaultOptions;
    var data;
    if (Array.isArray(evt.data.data)) {
      data = evt.data.data.map( function (x) { return JSON.parse(x); });
    } else {
      data = JSON.parse(evt.data.data);
    }
    if (evt.data.event == "restart") {
      currentData = [];
      data.forEach (function (datapoint) {
        datapoint.forEach(function (x, i) {
          currentData[i] = currentData[i] ? currentData[i] : [];
          currentData[i].push(x);
        });
      })
    } else if (evt.data.event == "datapoint") {
      if (!currentData) currentData = [];
      if (!Array.isArray(data)) data = [data];
      data.forEach(function (x, i) {
        currentData[i] = currentData[i] ? currentData[i] : [];
        currentData[i].push(x);
      });
    } else {
      if (!(data[0] && Array.isArray(data[0]))) data = [data];
      newOptions.series = []
      currentData = data;
    }
    chart.setOption(applyData(newOptions, currentData));
  });
}());
