/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef CALLQUEUE_HPP_
#define CALLQUEUE_HPP_

// Templated definitions for CallQueue search functions.

//template<typename T> T* CallQueue::find_earliest() {
//	for (auto iter = call_queue.begin(); iter != call_queue.end(); iter++) {
//		auto call = dynamic_cast<T*>(*iter);
//		if (call != NULL)
//			return call;
//	}
//	return NULL;
//}
//
//template<typename T> T* CallQueue::find_earliest(int request) {
//	for (auto iter = call_queue.begin(); iter != call_queue.end(); iter++) {
//		auto call = dynamic_cast<T*>(*iter);
//		if (call != NULL && call->request == request)
//			return call;
//	}
//	return NULL;
//}
//
//template<typename T> T* CallQueue::find_latest() {
//	for (auto iter = call_queue.rbegin(); iter != call_queue.rend(); iter++) {
//		auto call = dynamic_cast<T*>(*iter);
//		if (dynamic_cast<T*>(call) != NULL)
//			return call;
//	}
//	return NULL;
//}
//
//template<typename T> T* CallQueue::find_latest(int request) {
//	for (auto iter = call_queue.rbegin(); iter != call_queue.rend(); iter++) {
//		auto call = dynamic_cast<T*>(*iter);
//		if (call != NULL && call->request == request)
//			return call;
//	}
//	return NULL;
//}

template<typename T> T* CallQueue::find_earliest() {
	for (auto iter = call_queue.begin(); iter != call_queue.end(); iter++) {
		if (T::id == (*iter)->id) return (T*)(*iter);
	}
	return nullptr;
}

template<typename T> T* CallQueue::find_latest() {
	for (auto iter = call_queue.rbegin(); iter != call_queue.rend(); iter++) {
		if (T::id == (*iter)->id) return (T*)(*iter);
	}
	return nullptr;
}

#endif /* CALLQUEUE_HPP_ */
