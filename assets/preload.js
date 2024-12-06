const { contextBridge, ipcRenderer } = require('electron/renderer')

contextBridge.exposeInMainWorld('electronAPI', {
    send: ipcRenderer.send})

window.addEventListener("DOMContentLoaded", function () {
    document.querySelectorAll("embed").forEach(function (embed){
        if (embed.getAttribute("type") === "application/pdf") {
            embed.remove()
            ipcRenderer.send("neomacs",{type: "pdf-buffer", src: embed.getAttribute("src")?.replace(
                                            /^about:blank/g, "") || window.location.href})}})
})
