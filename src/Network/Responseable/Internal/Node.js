'use strict';

function requestImpl(left) {
	return function(right) {
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
			request.auth = ':' +
				(request.auth || '') +
				request.password;
			delete request.password;
		}

		require('http').request(request, function(response) {
			var body = '';

			response.on('data', function(chunk) {
				body += chunk;
			});

			response.on('end', function() {
				callback(right({statusCode: response.statusCode,
					headers: response.headers,
					body: body}))();
			});
		}).on('error', function(error) {
			callback(left(error.message))();
		}).write(requestBody)
		.end();
	}}}}
}

exports.requestImpl = requestImpl;
