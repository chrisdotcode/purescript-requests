'use strict';

function requestImpl(errback) {
	return function(callback) {
	return function(request) {
	return function() {
		var requestBody = request.body;
		delete request.body;

		if (request.user) {
			request.auth = user;
			delete request.user;
		}

		if (request.password) {
			// XXX use Node.js' internal/url.encodeAuth
			request.auth = (request.auth || '')
				+ ':'
				+ request.password;
			delete request.password;
		}

		// For some very odd reason, Node's `http.request` decides it
		// won't accept 'https:' as the protocol, and crashes (instead
		// of just choosing the correct protocol on-the-fly)...
		// Therefore, we have to conditionally require the module we
		// want based on the protocol that the user chose.
		var http;
		switch (request.protocol) {
		case 'http:':
			http = require('http');
			break;
		case 'https:':
			http = require('https');
			break;
		default:
			// XXX For now, the only supported modules are http and
			// https.
			http = require('http');
			break;
		}

		var nodeRequest = http.request(request, function(response) {
			var body = '';

			response.on('data', function(chunk) {
				body += chunk; // XXX worry about quadratic timing?
			});

			response.on('end', function() {
				callback({statusCode: response.statusCode,
					headers: response.headers,
					body: body})();
			});
		});

		nodeRequest.on('error', function(error) {
			errback(error)();
		});

		nodeRequest.write(requestBody);
		nodeRequest.end();
	}}}
}

exports.requestImpl = requestImpl;
