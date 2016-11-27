(() => {
    'use strict';

    let ws = null;
    let clients = {};
    let roomNumber = 0;
    let mediaStream = null;
    let serverConnected = false;
    let roomCreated = false;


    function createRoom (ws, roomNumber) {
        ws.send(JSON.stringify({
            action: 'create',
            room: roomNumber
        }));
    }

    function getMedia(video) {
        navigator.getUserMedia({
            video: true,
            audio: true
        }, (stream) => {
            if (window.URL) {
                video.src = window.URL.createObjectURL(stream);
            } else {
                video.src = stream;
            }

            mediaStream = stream;

            video.play();
        }, log);
    }

    function initClientPeer(name, client, room) {
        if(!mediaStream) {
            return false;
        }

        var index = 0;
        client.peer.addStream(mediaStream);
        client.peer.onicecandidate = function(event) {
            if (event.candidate) {
                client.candidates[index++] = event.candidate;
            }
        };

        client.peer.createOffer().then((offer) => {
            client.peer.setLocalDescription(offer).then(() => {
                ws.send(JSON.stringify({
                    action: 'offer',
                    room: room,
                    client: name,
                    sdp: client.peer.localDescription
                }));
            });
        });
    }


    window.onload = () => {


        const video = document.querySelector('video');
        const buttonLive = document.querySelector('.btn-live');
        const inputNumber = document.querySelector('.room');

        ws = new WebSocket(wsServer);

        ws.addEventListener('open', () => {serverConnected = true});

        buttonLive.addEventListener('click', () => {
            if (!serverConnected) {
                alert("Signaling server is unavailable!");
                return false;
            }

            roomNumber = parseInt(inputNumber.value, 10);
            createRoom(ws, roomNumber);
            getMedia(video);

        });

        ws.onmessage = (event) => {
            let data = JSON.parse(event.data);
            switch (data.code) {
                case STATUS_ROOM_CREATED:
                    roomCreated = true;
                    log('Room created successfully.');
                    break;
                case EVENT_CLIENT_ENTER:
                    clients[data.message] = {
                        peer: new RTCPeerConnection(configuration),
                        candidates: []
                    };
                    initClientPeer(data.message, clients[data.message], roomNumber);
                    break;
                case EVENT_ANSWER_RECEIVED:
                    var clientName = data.message.client;
                    var sdp = data.message.sdp;
                    var client = clients[clientName];
                    client.peer.setRemoteDescription(new SessionDescription(sdp))
                                        .then(() => {
                                            for (var i = 0; i < client.candidates.length; ++i) {
                                                ws.send(JSON.stringify({
                                                    action: 'server_ice',
                                                    room: roomNumber,
                                                    client: clientName,
                                                    candidate: client.candidates[i]
                                                }));
                                            }
                                        });
                    break;
                case EVENT_CLIENT_CANDIDATE_RECEIVED:
                    var clientName = data.message.client;
                    var candidate = data.message.candidate;

                    clients[clientName].peer.addIceCandidate(new IceCandidate({
                        sdpMLineIndex: candidate.sdpMLineIndex,
                        candidate: candidate.candidate
                    }));
                    break;
                case STATUS_SERVER_CANDIDATE_SENT:
                    log('Server\'s candidate sent.');
                    break;
                default:
                    log(data.message);

            }
        }

    }
})();

