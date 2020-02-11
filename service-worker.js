// Listen for install event, set callback
self.addEventListener('install', function(event) {
    console.log("Service worker installation.");
});


self.addEventListener('activate', function(event) {
    console.log("Service worker activation.");
});

