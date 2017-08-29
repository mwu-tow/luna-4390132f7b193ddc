/** @babel */
/** @jsx etch.dom */

var ProjectItem;

etch = require('etch');

module.exports = ProjectItem = class ProjectItem {
    constructor(name,
            uri = name,
            onOpen = (() => { return atom.project.setPaths([uri]); })) {
        this.update = this.update.bind(this);
        this.render = this.render.bind(this);
        this.name = name;
        this.uri = uri;
        this.onOpen = onOpen;
        etch.initialize(this);
    }

    update(props, children) {
        return etch.update(this);
    }

    render () {
        return <div class="inline-block btn" on={{click: this.onOpen}}>{this.name}</div>
    }
};
