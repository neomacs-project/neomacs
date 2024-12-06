const electron = require('electron');
const {app,net,WebContentsView,BaseWindow,
       protocol,clipboard,nativeTheme,ipcMain} = electron;
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
    const win = new BaseWindow(options);
    Ceramic.frames[id] = win;
    const root = Ceramic.buffers[id];
    win.contentView.addChildView(root);
    win.on('closed',()=>{
        RemoteJS.send(JSON.stringify({inputEvent: {type:'frame-closed'}, buffer: id}))})
    const resize = function (){
        setTimeout(function(){
        if(win.isDestroyed()) return;
        const bounds = win.getContentBounds();
        root.setBounds({x:0,y:0,width:bounds.width,height:bounds.height});
        root.webContents.executeJavaScript(`{const result={};
Array.from(document.getElementsByClassName("content")).forEach((c)=>{
    const rect = c.getBoundingClientRect();
    result[c.getAttribute("buffer")]={x:rect.x,y:rect.y,width:rect.width,height:rect.height}});
result}`).then((result)=>{
    for (buffer in result){
        const view = Ceramic.buffers[buffer];
        if(view) view.setBounds(result[buffer]);}})})};
    const focus = function (){
        RemoteJS.send(JSON.stringify({inputEvent: {type: "frame-focused"}, buffer: id}))};
    win.setMenu(null);
    win.on("resize",resize);
    win.on("maximize",resize);
    win.on("unmaximize",resize);
    win.on("enter-full-screen",resize);
    win.on("leave-full-screen",resize);
    win.on("show",resize);
    win.on("show",focus);
    win.on("restore",resize);
    win.on("restore",focus);
    win.on("focus",resize);
    win.on("focus",focus);
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
    buf.webContents.neomacsId = id;
    buf.webContents.focusRequested = false;
    buf.webContents.setMaxListeners(100);
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
    // The following 2 events are just for displaying spinners
    buf.webContents.on('did-start-loading',(details) =>{
        RemoteJS.send(JSON.stringify({inputEvent: {type: "did-start-loading"}, buffer: id}));});
    buf.webContents.on('did-stop-loading',(details) =>{
        RemoteJS.send(JSON.stringify({inputEvent: {type: "did-stop-loading"}, buffer: id}));});
    buf.webContents.on('focus',()=>{
        // avoid dead loop: lisp => webContents.focus => on 'focus' => 'focus' event (lisp) => lisp
        if(buf.webContents.focusRequested){
            buf.webContents.focusRequested = false;}
        else {
            RemoteJS.send(JSON.stringify({inputEvent: {type: "focus"}, buffer: id}));}});
    buf.webContents.on('enter-html-full-screen',()=>{
        RemoteJS.send(JSON.stringify({inputEvent: {type: "enter-html-full-screen"}, buffer: id}));});
    buf.webContents.on('leave-html-full-screen',()=>{
        RemoteJS.send(JSON.stringify({inputEvent: {type: "leave-html-full-screen"}, buffer: id}));});
    buf.webContents.loadURL(url).then(()=>
        {RemoteJS.send(JSON.stringify({inputEvent: {type: "load", url: url}, buffer: id}));},
        (err)=>{RemoteJS.send(JSON.stringify({inputEvent: {type: "fail-load", url: url, err: err}, buffer: id}));});
    buf.webContents.setWindowOpenHandler((details) => {
        return {
            action: 'allow',
            outlivesOpener: true,
            createWindow: (options) =>{
                const newId = Ceramic.generateBufferId();
                const newOptions = {}; // Have to filter out junk in options
                if(options.webContents) newOptions.webContents = options.webContents;
                if(options.webPreferences) newOptions.webPreferences = options.webPreferences;
                const newBuffer = Ceramic.createBuffer(newId, details.url, newOptions);
                RemoteJS.send(JSON.stringify({inputEvent: {type: "new-buffer", newId: newId, url: details.url}, buffer: id}));
                return newBuffer;}}});
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

/* Start up */

var Contents = {};
var Mounts = {};

app.on('ready', function() {
    // Start the WebSockets server
    Ceramic.startWebSockets(process.argv[2],
                            parseInt(process.argv[3]));
    ipcMain.on('neomacs',(event, details) => {
        RemoteJS.send(JSON.stringify({inputEvent: {type: "ipc", details: details}, buffer: event.sender.neomacsId}));})
    protocol.handle('neomacs', (req) => {
        const {host, pathname} = new URL(req.url);
        p = pathname.substring(1);
        if (host == 'contents'){
            const content = Contents[p];
            delete Contents[p];
            return new Response(content,{headers: {'content-type': "text/html; charset=utf-8"}});
        }
        else if (host == 'null.contents'){
            return new Response("<!DOCTYPE html><html><head></head><body></body></html>",
                                {headers: {'content-type': "text/html; charset=utf-8"}});
        }
        else{
            const pathToServe = path.resolve(Mounts[host],p);
            return net.fetch(pathToFileURL(pathToServe).toString());}});});

//process.removeAllListeners("uncaughtExceptions");
process.on("uncaughtException", (err) => {
    if(err.message === "Invalid webContents. Created window should be connected to webContents passed with options object.")
        return;
    dialog.showMessageBoxSync({type: "error",
                               title: "Error in Electron main process",
                               message: err.stack});})
