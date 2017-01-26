'use strict';

function determinePlatformImpl(Node) {
	return function(Browser) {
	return function(Other) {
		// https://github.com/iliakan/detect-node/blob/802d4484821b8dbf0e64cb0f7694c2961904cb89/index.js
		return Object.prototype.toString.call(typeof process !== 'undefined' ? process : 0) === '[object process]'
			? Node
			: Browser; // XXX Other is currently unimplemented.
	}}
}

function foreignHeadersToHeadersImpl(tuple) {
	return function(headerNameFromString) {
	return function(HVStr) {
	return function(HVList) {
	return function(headers) {
		var tuplePairs = [];

		for (var headerName in headers) {
			var headerValue = headers[headerName];

			// XXX I suppose it's possible that we *could*
			// eventually parse other headers that might
			// be lists in addition to 'Set-Cookie'. (When
			// we do, it should be done in PureScript,
			// though.)
			if (headerName === 'set-cookie') {
				headerValue = HVList(headerValue);
			} else {
				headerValue = HVStr(headerValue);
			}

			tuplePairs.push(
				tuple(headerNameFromString(headerName))
				     (headerValue)
			);
		}
		return tuplePairs;
	}}}}
}

exports.determinePlatformImpl       = determinePlatformImpl;
exports.foreignHeadersToHeadersImpl = foreignHeadersToHeadersImpl;
