const electron = require('electron');
const {app,net,WebContentsView,BaseWindow,
       protocol,clipboard,nativeTheme} = electron;
const WebSocket = require('ws');
const dialog = require('electron').dialog;
const { pathToFileURL } = require('url')
const path = require('node:path')

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
    Promise.race([Promise.resolve(fn()),
                  new Promise((resolve,reject)=>{
                      setTimeout(()=>{reject('timeout')},200)})]
                ).then(function(result){
        RemoteJS.send(JSON.stringify({
            id: id,
            result: result
        }))},function(err){
            RemoteJS.send(JSON.stringify({
                id: id,
                result: null
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
    win.on('closed',()=>{
        RemoteJS.send(JSON.stringify({inputEvent: {type:'frame-closed'}, buffer: id}))})
    return win;
};

Ceramic.closeFrame = function(id) {
    const win = Ceramic.frames[id];
    if(!win.isDestroyed()) win.close();
    delete Ceramic.frames[id];
};

Ceramic.generateBufferId = function (){
    for(let i = -1;;i--){
        const id = i.toString();
        if(!(id in Ceramic.buffers)) return id;
    }}

Ceramic.createBuffer = function(id, url, options) {
    const buf = new WebContentsView(options);
    buf.ignoreKeys = [];
    buf.webContents.on('before-input-event', (event, input) => {
        const ignoreIndex = buf.ignoreKeys.findIndex((i) => true);
        if(ignoreIndex >= 0){
            buf.ignoreKeys.splice(ignoreIndex, 1);}
        else{RemoteJS.send(JSON.stringify({inputEvent: input, buffer: id}));
             event.preventDefault();}});
    buf.webContents.on('dom-ready', () => {
        RemoteJS.send(JSON.stringify({inputEvent: {type: "dom-ready"}, buffer: id}));});
    buf.webContents.on('page-title-updated', (event, title, explicitSet) => {
        RemoteJS.send(JSON.stringify({inputEvent: {type: "title-updated", title: title}, buffer: id}))});
    buf.webContents.on('did-start-navigation',(details) =>{
        RemoteJS.send(JSON.stringify({inputEvent: {type: "did-start-navigation", ...details}, buffer: id}));});
    buf.webContents.loadURL(url).then(()=>
        {RemoteJS.send(JSON.stringify({inputEvent: {type: "load", url: url}, buffer: id}));},
        (err)=>{RemoteJS.send(JSON.stringify({inputEvent: {type: "fail-load", url: url, err: err}, buffer: id}));});
    buf.webContents.setWindowOpenHandler((details) => {
        return {
            action: 'allow',
            outlivesOpener: true,
            createWindow: (options) =>{
                const newId = Ceramic.generateBufferId();
                RemoteJS.send(JSON.stringify({inputEvent: {type: "new-buffer", newId: newId, url: details.url}, buffer: id}));
                return Ceramic.createBuffer(newId, details.url, {});}}});
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

var Contents = {};
var Mounts = {};

app.on('ready', function() {
    // Start the WebSockets server
    Ceramic.startWebSockets(process.argv[2],
                            parseInt(process.argv[3]));
    protocol.handle('neomacs', (req) => {
        const {host, pathname} = new URL(req.url);
        p = pathname.substring(1);
        if (host == 'contents'){
            const content = Contents[p];
            delete Contents[p];
            return new Response(content,{headers: {'content-type': "text/html"}});
        }
        else{
            const pathToServe = path.resolve(Mounts[host],p);
            return net.fetch(pathToFileURL(pathToServe).toString());}});});
