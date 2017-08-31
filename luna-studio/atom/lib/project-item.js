/** @babel */
/** @jsx etch.dom */

var ProjectItem;

etch = require('etch');

module.exports = ProjectItem = class ProjectItem {
    constructor(
            project,
            classes = "",
            onOpen = (() => { return atom.project.setPaths([this.uri]); })) {
        this.update = this.update.bind(this);
        this.render = this.render.bind(this);
        this.name = project.name;
        this.uri = project.uri != undefined ? project.uri : project.name;
        this.classes = classes;
        this.onOpen = onOpen;
        this.thumb = project.thumb != undefined ? project.thumb : "atom://luna-studio/rsc/logo.png";
        etch.initialize(this);
    }

    update(props, children) {
        return etch.update(this);
    }

    render () {
        return  <div class={this.classes} on={{click: this.onOpen}}>
                    <img class="luna-project-logo" src={this.thumb}></img>
                    <div class="luna-project-caption">{this.name}</div>
                </div>
    }
};
