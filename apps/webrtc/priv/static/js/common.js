const ERROR_ROOM_ALREADY_EXISTED = -100;
const ERROR_USER_ALREADY_EXISTED = -101;
const ERROR_ROOM_DOES_NOT_EXIST = -102;
const ERROR_CLIENT_DOES_NOT_EXIST = -103;
const ERROR_ROOM_DOES_NOT_HAVE_ANY_CLIENT = -104;

    
const STATUS_UNSUCCESSFUL = -1;
const STATUS_UNSUPPORTED_OPERATION = 0;
const STATUS_ROOM_CREATED = 1;
const STATUS_ROOM_ENTERED = 2;
const STATUS_OFFER_SENT = 3;
const STATUS_ANSWER_SENT = 4;
const STATUS_SERVER_CANDIDATE_SENT = 5;
const STATUS_CLIENT_CANDIDATE_SENT = 6;
    
const EVENT_CLIENT_ENTER = 100;
const EVENT_OFFER_RECEIVED = 101;
const EVENT_ANSWER_RECEIVED = 102;
const EVENT_SERVER_CANDIDATE_RECEIVED = 103;
const EVENT_CLIENT_CANDIDATE_RECEIVED = 104;




const RTCPeerConnection = window.mozRTCPeerConnection || window.webkitRTCPeerConnection;
const IceCandidate = window.mozRTCIceCandidate || window.RTCIceCandidate;
const SessionDescription = window.mozRTCSessionDescription || window.RTCSessionDescription;
navigator.getUserMedia = navigator.getUserMedia || navigator.mozGetUserMedia || navigator.webkitGetUserMedia;


const wsServer = 'ws://' + window.location.host + '/websocket';

const configuration = {
    iceServers: [
        {urls: "stun:23.21.150.121"}
    ]
};


function log(text) {
    console.log((performance.now() / 1000).toFixed(3) + ' --> ' + text);
}