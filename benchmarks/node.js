var parser = require('engine.io-parser');

var run = function(fn) {
    var start = new Date();
    for (var i=0; i<10000; ++i) fn();
    return new Date - start;
};
var runNTimes = function(n, fn) {
    var allms = 0, ms;
    for (var i=0; i<n; ++i) {
        allms += run(fn);
    }
    return allms / n;
};
var bench = function(title, fn) {
    console.log(title + ': ' + runNTimes(5, fn) + ' milliseconds');
};

bench('text encoding', function() {
    parser.encodePacket({type: 'message', data: 'Hi, there'}, true, function(){});
});

var binaryData = new Int8Array(5);
for (var i = 0; i < binaryData.length; i++) {
    binaryData[i] = i;
}

bench('binary encoding', function() {
    parser.encodePacket({type: 'message', data: binaryData}, true, function(){});
});

bench('text decoding', function() {
    parser.decodePacket("2probe");
});

var binaryPacket = new Int8Array(6);
binaryPacket[0] = 4;
for (var i = 1; i < binaryPacket.length; i++) {
    binaryPacket[i] = i;
}

bench('binary decoding', function() {
    parser.decodePacket(binaryPacket);
});

bench('payload encoding', function() {
    parser.encodePayload([{type: 'message', data: binaryData}, {type: 'close'}], function(){});
});

bench('payload decoding', function() {
    parser.decodePayload("10:b4AAECAwQ=1:1", function(){});
});
