/** @babel */
/** @jsx etch.dom */
const $ = require('jquery');
var Spinner;

etch = require('etch');

module.exports = Spinner = class Spinner {
    constructor(overlap = false) {
        this.overlap = overlap;
        this.update = this.update.bind(this);
        this.render = this.render.bind(this);
        this.lastProgress = null;
        etch.initialize(this);
    }

    update(props, children) {
        return etch.update(this);
    }

    render() {
        if(this.overlap)
            return  <div id="luna_logo-spinner" class="luna_logo-overlap">
                      { this.renderSpinner() }
                    </div>
        else
            return  <div id="luna_logo-spinner">
                      { this.renderSpinner() }
                    </div>

    }
    renderSpinner () {
        return <div id="logo-area">
                  <div id="spinner">
                      <svg id="install-spinner" height="180" width="180" class="spinner__circle" shape-rendering="geometricPrecision" viewBox="0 0 180 180">
                          <circle class="logo-circle" id="progress-bg" r="85.78125" cx="90" cy="90" fill="transparent" style="stroke: rgb(103, 14, 29);"></circle>
                          <circle class="logo-circle" id="progress-bar" r="85.78125" cx="90" cy="90" fill="transparent"  style="stroke: rgb(207, 28, 59)"></circle>
                          <svg id="full-ring">
                              <circle id="full-ring-bg" r="85.78125" cx="90" cy="90" fill="transparent" ></circle>
                              <circle class="logo-circle" r="85.78125" cx="90" cy="90" fill="transparent" style="stroke: rgb(207, 28, 59);"></circle>
                          </svg>
                      </svg>
                  </div>
              </div>
    }

    start() {
        $('#full-ring').css('opacity', 0.0)
        $("#spinner").addClass("rotating")
    }
    setProgress(progress) {
        if(!this.lastProgress || this.lastProgress < progress) {
            var value = (0.1 + progress)/1.1;
            var dashValue = parseFloat($("#progress-bar").css('stroke-dasharray'));
            $("#progress-bar").css('stroke-dashoffset', (dashValue - value * dashValue));
            this.lastProgress = progress;
        }
    }
    finish() {
        $("#spinner").removeClass("rotating")
        $('#full-ring').css('opacity', 1.0)
    }
};
