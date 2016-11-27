(() => {
    'use strict';

    let serverConnected = false;
    let roomNumber = 0;
    let userName = null;
    let video = null;

    function enterRoom(ws, room, name) {
        ws.send(JSON.stringify({
            action: 'enter',
            room: room,
            client: name
        }));
    }

    function setUpConnection(ws, peer) {
        peer.onicecandidate = (event) => {
            if (event.candidate) {
                ws.send(JSON.stringify({
                    action: 'client_ice',
                    room: roomNumber,
                    client: userName,
                    candidate: event.candidate
                }));
            }
        };

        peer.onaddstream = (event) => {
            if (!event) {
                return
            }

            if (window.URL) {
                video.src = window.URL.createObjectURL(event.stream);
            } else {
                video.src = event.stream;
            }

            video.play();
        }
    }

    window.onload = () => {

        const ws = new WebSocket(wsServer);

        let peerConnection = null;
        const buttonWatch = document.querySelector('.btn-watch');
        const inputUser = document.querySelector('.user');
        const inputNumber = document.querySelector('.room');
        video = document.querySelector('video');

        ws.addEventListener('open', () => {serverConnected = true});

        buttonWatch.addEventListener('click', () => {
            if (!serverConnected) {
                alert("Signaling server is unavailable!");
                return false;
            }

            roomNumber = parseInt(inputNumber.value, 10);
            userName   = inputUser.value;

            peerConnection = new RTCPeerConnection(configuration);

            enterRoom(ws, roomNumber, userName);
            setUpConnection(ws, peerConnection);

        });


        ws.onmessage = (event) => {
            let data = JSON.parse(event.data);

            switch (data.code) {
                case EVENT_OFFER_RECEIVED:
                    var sessionDescription = new SessionDescription(data.message);
                    peerConnection.setRemoteDescription(sessionDescription).then(() => {
                        peerConnection.createAnswer().then((answer) => {
                            return peerConnection.setLocalDescription(answer);
                        }).then(() => {
                            ws.send(JSON.stringify({
                                action: 'answer',
                                room: roomNumber,
                                client: userName,
                                sdp: peerConnection.localDescription
                            }));
                        });
                    });
                    break;
                case EVENT_SERVER_CANDIDATE_RECEIVED:
                    peerConnection.addIceCandidate(new IceCandidate({
                        sdpMLineIndex: data.message.sdpMLineIndex,
                        candidate: data.message.candidate
                    }));
                    break;
                case STATUS_ANSWER_SENT:
                    log('Answer sent');
                    break;
                case STATUS_CLIENT_CANDIDATE_SENT:
                    log('Client\'s candidate sent');
                    break;
                default:
                    log(data.message);
            }
        };
    }
})();