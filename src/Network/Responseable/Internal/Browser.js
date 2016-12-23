'use strict';

// I should steal and modify the code from: https://github.com/nodejs/node/blob/accf410eb0992628b4aab3c139242a409874a4fb/lib/url.js#L558-L637
function urlFormat(request) {
	return request.protocol + '//' + request.hostname + ':' + request.port + request.path;
}

// Stolen from https://gist.github.com/monsur/706839
function parseHeader(headersStr) {
	var headers = {};

	var headerPairs = headersStr.split('\u000d\u000a'); // \r\n

	for (var i = 0; i < headerPairs.length; i++) {
		var headerPair = headerPairs[i];
		// Can't use split() here because it does the wrong thing
		// if the header value has the string ": " in it.
		var index = headerPair.indexOf('\u003a\u0020');
		if (index > 0) {
			var key = headerPair.substring(0, index);
			var val = headerPair.substring(index + 2);
			_addHeaderLine(key, val, headers);
		}
	}
	return headers;
}

// This is used here so that the header list looks like the Node.js header list. Stolen from https://github.com/nodejs/node/blob/master/lib/_http_incoming.js#L134-L181
function _addHeaderLine(field, value, dest) {
	field = field.toLowerCase();
	switch (field) {
		// Array headers:
		case 'set-cookie':
			if (dest[field] !== undefined) {
				dest[field].push(value);
			} else {
				dest[field] = [value];
			}
			break;

		/* eslint-disable max-len */
		// list is taken from:
		// https://mxr.mozilla.org/mozilla/source/netwerk/protocol/http/src/nsHttpHeaderArray.cpp
		/* eslint-enable max-len */
		case 'content-type':
		case 'content-length':
		case 'user-agent':
		case 'referer':
		case 'host':
		case 'authorization':
		case 'proxy-authorization':
		case 'if-modified-since':
		case 'if-unmodified-since':
		case 'from':
		case 'location':
		case 'max-forwards':
		case 'retry-after':
		case 'etag':
		case 'last-modified':
		case 'server':
		case 'age':
		case 'expires':
			// drop duplicates
			if (dest[field] === undefined)
				dest[field] = value;
			break;

		default:
			// make comma-separated list
			if (typeof dest[field] === 'string') {
				dest[field] += ', ' + value;
			} else {
				dest[field] = value;
			}
	}
}

function requestImpl(left) {
	return function(right) {
	return function(callback) {
	return function(request) {
	return function() {
		var browserRequest = new XMLHttpRequest();

		browserRequest.open(request.method, urlFormat(request), true, request.user, request.password);

		for (var key in request.headers) {
			browserRequest.setRequestHeader(key,
				request.headers[key]);
		}

		if (request.timeout) {
			browserRequest.timeout = request.timeout;
		}

		browserRequest.addEventListener('load', function() {
			var responseText = this.responseText;
			var statusCode   = this.status;
			var headers      = this.getAllResponseHeaders();

			callback(right({statusCode: statusCode,
				headers: parseHeaders(headers),
				body: responseText}))();
		});

		browserRequest.addEventListener('error', function(error) {
			callback(left(error))();
		});

		browserRequest.send(request.body);
	}}}}
}

exports.requestImpl = requestImpl;
