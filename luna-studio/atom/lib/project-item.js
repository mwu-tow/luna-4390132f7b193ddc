/** @babel */
/** @jsx etch.dom */
const Spinner = require('./spinner');

var ProjectItem;

etch = require('etch');

module.exports = ProjectItem = class ProjectItem {
    constructor(project, classes = "", onOpen) {
        this.update = this.update.bind(this);
        this.render = this.render.bind(this);
        this.thumbLogo = this.thumbLogo.bind(this);
        this.name = project.name;
        this.uri = project.uri != undefined ? project.uri : project.name;
        this.classes = classes;
        this.onOpen = function() {
            spinner = new Spinner(progress = 0)
            var self = this;
            update = function() {
                self.update( { logo: spinner.render() } );
            }
            progress = function(p) {
                spinner.setProgress(p);
                update();
            };
            finalize = function() {
                self.update( { logo: self.thumbLogo() } );
            }
            update();
            onOpen(progress, finalize);
        };
        this.thumb = project.thumb != undefined ? project.thumb : "atom://luna-studio/rsc/logo.png";
        this.props = { logo: this.thumbLogo() }
        etch.initialize(this);
    }

    update(props, children) {
        this.props = props;
        return etch.update(this);
    }
    thumbLogo() {
        return <img class="luna-project-logo" src={this.thumb}></img>
    }
    render () {
        return  <div class={this.classes} on={{click: this.onOpen}}>
                    {this.props.logo}
                    <div class="luna-project-caption">{this.name}</div>
                </div>
    }
};
