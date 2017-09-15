/** @babel */
/** @jsx etch.dom */
const Spinner = require('./spinner');

var ProjectItem;

etch = require('etch');

module.exports = ProjectItem = class ProjectItem {
    constructor(
            project,
            classes = "",
            onOpen = ((progress, finalize) => {
                progress(0.5);
                atom.project.setPaths([this.uri]);
                finalize();
            })) {
        this.update = this.update.bind(this);
        this.render = this.render.bind(this);
        this.thumbLogo = this.thumbLogo.bind(this);
        this.name = project.name;
        this.uri = project.uri != undefined ? project.uri : project.name;
        this.classes = classes;
        this.onOpen = function() {
            spinner = new Spinner()
            this.update( { logo: spinner.render() } );
            etch.updateSync(this);
            spinner.start();
            etch.updateSync(this);
            progress = function(p) {
                // etch.updateSync(this);
                spinner.setProgress(p);
            };
            var self = this;
            finalize = function() {
                self.update( { logo: self.thumbLogo() } );
            }
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
