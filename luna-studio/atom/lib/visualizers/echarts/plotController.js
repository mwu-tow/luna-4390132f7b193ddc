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
          icon: 'path://M3.5 18.5l6-6 4 4L22 7l-1.4-1.5-7 8-4-4L2 17l1.5 1.5z',
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
          icon: 'path://M3 19h18V6l-7.5 7.6-4.2-3.8L3 16.2V19z',
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
          icon: 'path://M13 19h-2v-7h2v7zm-4 0H7V9h2v10zm-4 0H3v-7h2v7zm14 0V5h2v14h-2zm-2 0h-2V9h2v10z',
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
          icon: 'path://M21 8c.6 0 1-.4 1-1s-.4-1-1-1-1 .4-1 1 .4 1 1 1zM5 16c.6 0 1-.4 1-1s-.4-1-1-1-1 .4-1 1 .4 1 1 1zm2-2c.6 0 1-.4 1-1s-.4-1-1-1-1 .4-1 1 .4 1 1 1zm2-2c.6 0 1-.4 1-1s-.4-1-1-1-1 .4-1 1 .4 1 1 1zm2 2c.6 0 1-.4 1-1s-.4-1-1-1-1 .4-1 1 .4 1 1 1zm8-4c.6 0 1-.4 1-1s-.4-1-1-1-1 .4-1 1 .4 1 1 1zm-2 2c.6 0 1-.4 1-1s-.4-1-1-1-1 .4-1 1 .4 1 1 1zm-4 4c.6 0 1-.4 1-1s-.4-1-1-1-1 .4-1 1 .4 1 1 1zm2-2c.6 0 1-.4 1-1s-.4-1-1-1-1 .4-1 1 .4 1 1 1zM3 18c.6 0 1-.4 1-1s-.4-1-1-1-1 .4-1 1 .4 1 1 1z',
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
          icon: 'path://M14 16h-4V9h4v7zm-7 3H3v-7h4v7zm14-7h-4V5h4v7z',
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

        // myResetView: {
        //   show: true,
        //   title: 'reset view',
        //   icon: 'path://M12,3 C7.03,3 3,7.03 3,12 L0,12 L4,16 L8,12 L5,12 C5,8.13 8.13,5 12,5 C15.87,5 19,8.13 19,12 C19,15.87 15.87,19 12,19 C10.49,19 9.09,18.51 7.94,17.7 L6.52,19.14 C8.04,20.3 9.94,21 12,21 C16.97,21 21,16.97 21,12 C21,7.03 16.97,3 12,3 L12,3 Z',
        //   onclick: function (){
        //     options = chart.getOption();
        //     options.legend   = defaultOptions.legend;
        //     options.dataZoom = defaultOptions.dataZoom;
        //     chart.setOption(addLegendData(options), { notMerge: true });
        //   }
        // }
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
