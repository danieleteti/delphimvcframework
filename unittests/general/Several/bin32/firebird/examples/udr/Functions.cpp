/*
 *  The contents of this file are subject to the Initial
 *  Developer's Public License Version 1.0 (the "License");
 *  you may not use this file except in compliance with the
 *  License. You may obtain a copy of the License at
 *  http://www.ibphoenix.com/main.nfs?a=ibphoenix&page=ibp_idpl.
 *
 *  Software distributed under the License is distributed AS IS,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied.
 *  See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Original Code was created by Adriano dos Santos Fernandes
 *  for the Firebird Open Source RDBMS project.
 *
 *  Copyright (c) 2008 Adriano dos Santos Fernandes <adrianosf@gmail.com>
 *  and all contributors signed below.
 *
 *  All Rights Reserved.
 *  Contributor(s): ______________________________________.
 */

#include "UdrCppExample.h"

using namespace Firebird;


//------------------------------------------------------------------------------


/***
create function wait_event (
    event_name varchar(31) character set utf8 not null
) returns integer not null
    external name 'udrcpp_example!wait_event'
    engine udr;
***/
FB_UDR_BEGIN_FUNCTION(wait_event)
	FB_UDR_MESSAGE(InMessage,
		(FB_VARCHAR(31 * 4), name)
	);

	FB_UDR_MESSAGE(OutMessage,
		(FB_INTEGER, result)
	);

	FB_UDR_EXECUTE_FUNCTION
	{
		char* s = new char[in->name.length + 1];
		memcpy(s, in->name.str, in->name.length);
		s[in->name.length] = '\0';

		unsigned char* eveBuffer;
		unsigned char* eveResult;
		int eveLen = isc_event_block(&eveBuffer, &eveResult, 1, s);

		delete [] s;

		ISC_STATUS_ARRAY statusVector = {0};
		isc_db_handle dbHandle = Helper::getIscDbHandle(status, context);
		ISC_ULONG counter = 0;

		FbException::check(isc_wait_for_event(
			statusVector, &dbHandle, eveLen, eveBuffer, eveResult), status, statusVector);
		isc_event_counts(&counter, eveLen, eveBuffer, eveResult);
		FbException::check(isc_wait_for_event(
			statusVector, &dbHandle, eveLen, eveBuffer, eveResult), status, statusVector);
		isc_event_counts(&counter, eveLen, eveBuffer, eveResult);

		isc_free((char*) eveBuffer);
		isc_free((char*) eveResult);

		out->resultNull = FB_FALSE;
		out->result = counter;
	}
FB_UDR_END_FUNCTION


/***
create function sum_args (
    n1 integer,
    n2 integer,
    n3 integer
) returns integer
    external name 'udrcpp_example!sum_args'
    engine udr;
***/
FB_UDR_BEGIN_FUNCTION(sum_args)
	// Without InMessage/OutMessage definitions, messages will be byte-based.

	FB_UDR_CONSTRUCTOR
		// , inCount(0)
	{
		// Get input metadata.
		AutoRelease<IMessageMetadata> inMetadata(metadata->getInputMetadata(status));

		// Get count of input parameters.
		inCount = inMetadata->getCount(status);

		inNullOffsets.reset(new unsigned[inCount]);
		inOffsets.reset(new unsigned[inCount]);

		for (unsigned i = 0; i < inCount; ++i)
		{
			// Get null offset of the i-th input parameter.
			inNullOffsets[i] = inMetadata->getNullOffset(status, i);

			// Get the offset of the i-th input parameter.
			inOffsets[i] = inMetadata->getOffset(status, i);
		}

		// Get output metadata.
		AutoRelease<IMessageMetadata> outMetadata(metadata->getOutputMetadata(status));

		// Get null offset of the return value.
		outNullOffset = outMetadata->getNullOffset(status, 0);

		// Get offset of the return value.
		outOffset = outMetadata->getOffset(status, 0);
	}

	// This function requires the INTEGER parameters and return value, otherwise it will crash.
	// Metadata is inspected dynamically (in execute). This is not the fastest method.
	FB_UDR_EXECUTE_FUNCTION
	{
		*(ISC_SHORT*) (out + outNullOffset) = FB_FALSE;

		// Get a reference to the return value.
		ISC_LONG& ret = *(ISC_LONG*) (out + outOffset);

		// The return value is automatically initialized to 0.
		///ret = 0;

		for (unsigned i = 0; i < inCount; ++i)
		{
			// If the i-th input parameter is NULL, set the output to NULL and finish.
			if (*(ISC_SHORT*) (in + inNullOffsets[i]))
			{
				*(ISC_SHORT*) (out + outNullOffset) = FB_TRUE;
				return;
			}

			// Read the i-th input parameter value and sum it in the referenced return value.
			ret += *(ISC_LONG*) (in + inOffsets[i]);
		}
	}

	unsigned inCount;
	AutoArrayDelete<unsigned> inNullOffsets;
	AutoArrayDelete<unsigned> inOffsets;
	unsigned outNullOffset;
	unsigned outOffset;
FB_UDR_END_FUNCTION


//------------------------------------------------------------------------------


// This should be used in only one of the UDR library files.
// Build must export firebird_udr_plugin function.
FB_UDR_IMPLEMENT_ENTRY_POINT
