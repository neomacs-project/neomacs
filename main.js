const electron = require('electron');
const {app,WebContentsView,BaseWindow} = electron;
const WebSocket = require('ws');
const dialog = require('electron').dialog;

var Ceramic = {
    dialog: dialog
};

electron.powerSaveBlocker.start('prevent-app-suspension');

/* Communication */

var RemoteJS = {};

Ceramic.startWebSockets = function(address, port) {
    RemoteJS.ws = new WebSocket('ws://' + address + ':' + port);

    RemoteJS.send = function(data) {
        RemoteJS.ws.send(data);
    };

    RemoteJS.ws.onmessage = function(evt) {
        const js = evt.data;
        try {
            eval(js);
        } catch (err) {
            dialog.showErrorBox('JavaScript Error: ' + err, 'Error evaluating JavaScript from Ceramic: ' + js);
        }
    };
    RemoteJS.ws.onopen = function() {
        RemoteJS.send('connected');
    };
};

Ceramic.syncEval = function(id, fn) {
    Promise.resolve(fn()).then(function(result){
        RemoteJS.send(JSON.stringify({
            id: id,
            result: result
        }))})};

Ceramic.startCrashReporter = function (options) {
    electron.crashReporter.start(options);
};

/* Windows */

Ceramic.frames = {};
Ceramic.buffers = {};

Ceramic.createFrame = function(id, options) {
    var win = new BaseWindow(options);
    Ceramic.frames[id] = win;
    return win;
};

Ceramic.closeFrame = function(id) {
    Ceramic.frames[id].close()
    Ceramic.frames[id] = null;
};

Ceramic.createBuffer = function(id, url, options) {
    var buf = new WebContentsView(options);
    buf.webContents.on('before-input-event', (event, input) => {
        RemoteJS.send(JSON.stringify({'inputEvent': input, 'buffer': id}));
        event.preventDefault(); });
    buf.webContents.on('did-finish-load', () => {
        RemoteJS.send(JSON.stringify({'inputEvent': {'type':"load"}, 'buffer': id}));});
    buf.webContents.loadURL(url);
    Ceramic.buffers[id] = buf;
    return buf;
};

Ceramic.closeBuffer = function(id) {
    Ceramic.buffers[id].webContents.close();
    Ceramic.buffers[id] = null;
};

/* Lifecycle management */

Ceramic.quit = function() {
    app.quit();
};

app.on('window-all-closed', function() {
    if (process.platform != 'darwin') {
        // FIXME: signal that everything's closed
    }
    app.quit();
});

/* Start up */

app.on('ready', function() {
    // Start the WebSockets server
    Ceramic.startWebSockets(process.argv[2],
                            parseInt(process.argv[3]));
});
