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
	};};};};
}

function fromTupleImpl(fst) {
	return function(snd) {
	return function(pairs) {
		var o = {};
		// pairs is a PureScript Tuple list, not a JavaScript key:value
		// Object, so it needs to be iterated with this variant of for.
		for  (var i = 0, len = pairs.length; i < len; i++) {
			var pair  = pairs[i];
			var key   = fst(pair)();
			var value = snd(pair)();
			o[key] = value;
		}
		return o;
	};};
};

exports.undefined                   = undefined;
exports.determinePlatformImpl       = determinePlatformImpl;
exports.foreignHeadersToHeadersImpl = foreignHeadersToHeadersImpl;
exports.fromTupleImpl               = fromTupleImpl;
