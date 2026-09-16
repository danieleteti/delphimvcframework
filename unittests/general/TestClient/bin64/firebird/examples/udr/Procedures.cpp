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
create procedure gen_rows (
    start_n integer not null,
    end_n integer not null
) returns (
    n integer not null
)
    external name 'udrcpp_example!gen_rows'
    engine udr;
***/
FB_UDR_BEGIN_PROCEDURE(gen_rows)
	// Without InMessage/OutMessage definitions, messages will be byte-based.

	// Procedure variables.
	unsigned inOffsetStart, inOffsetEnd, outNullOffset, outOffset;

	// Get offsets once per procedure.
	FB_UDR_CONSTRUCTOR
	{
		AutoRelease<IMessageMetadata> inMetadata(metadata->getInputMetadata(status));

		inOffsetStart = inMetadata->getOffset(status, 0);
		inOffsetEnd = inMetadata->getOffset(status, 1);

		AutoRelease<IMessageMetadata> outMetadata(metadata->getOutputMetadata(status));

		outNullOffset = outMetadata->getNullOffset(status, 0);
		outOffset = outMetadata->getOffset(status, 0);
	}

	/*** Procedure destructor.
	FB_UDR_DESTRUCTOR
	{
	}
	***/

	FB_UDR_EXECUTE_PROCEDURE
	{
		counter = *(ISC_LONG*) (in + procedure->inOffsetStart);
		end = *(ISC_LONG*) (in + procedure->inOffsetEnd);

		*(ISC_SHORT*) (out + procedure->outNullOffset) = FB_FALSE;
	}

	// After procedure's execute definition, starts the result set definition.

	FB_UDR_FETCH_PROCEDURE
	{
		if (counter > end)
			return false;

		*(ISC_LONG*) (out + procedure->outOffset) = counter++;
		return true;
	}

	/*** ResultSet destructor.
	~ResultSet()
	{
	}
	***/

	// ResultSet variables.
	ISC_LONG counter;
	ISC_LONG end;
FB_UDR_END_PROCEDURE


/***
create procedure gen_rows2 (
    start_n integer not null,
    end_n integer not null
) returns (
    n integer not null
)
    external name 'udrcpp_example!gen_rows2'
    engine udr;
***/
FB_UDR_BEGIN_PROCEDURE(gen_rows2)
	FB_UDR_MESSAGE(InMessage,
		(FB_INTEGER, start)
		(FB_INTEGER, end)
	);

	FB_UDR_MESSAGE(OutMessage,
		(FB_INTEGER, result)
	);

	FB_UDR_EXECUTE_PROCEDURE
	{
		out->resultNull = FB_FALSE;
		out->result = in->start - 1;
	}

	FB_UDR_FETCH_PROCEDURE
	{
		return out->result++ < in->end;
	}
FB_UDR_END_PROCEDURE


/***
create procedure inc (
    count_n integer not null
) returns (
    n0 integer not null,
    n1 integer not null,
    n2 integer not null,
    n3 integer not null,
    n4 integer not null
)
    external name 'udrcpp_example!inc'
    engine udr;
***/
// This is a sample procedure demonstrating how the scopes of variables works.
// n1 and n2 are on the Procedure scope, i.e., they're shared for each execution of the same cached
// metadata object.
// n3 and n4 are on the ResultSet scope, i.e., each procedure execution have they own instances.
FB_UDR_BEGIN_PROCEDURE(inc)
	FB_UDR_MESSAGE(InMessage,
		(FB_INTEGER, count)
	);

	FB_UDR_MESSAGE(OutMessage,
		(FB_INTEGER, n0)
		(FB_INTEGER, n1)
		(FB_INTEGER, n2)
		(FB_INTEGER, n3)
		(FB_INTEGER, n4)
	);

	ISC_LONG n1;

	// This is how a procedure (class) initializer is written.
	// ResultSet variables are not accessible here.
	// If there is nothing to initialize, it can be completelly suppressed.
	FB_UDR_CONSTRUCTOR
		, n1(0),
		  n2(0)
	{
	}

	ISC_LONG n2;

	// FB_UDR_EXECUTE_PROCEDURE starts the ResultSet scope.
	FB_UDR_EXECUTE_PROCEDURE
		// This is the ResultSet (class) initializer.
		, n3(procedure->n1),	// n3 will start with the next value for n1 of the last execution
		  n4(0)
	{
		out->n0Null = out->n1Null = out->n2Null = out->n3Null = out->n4Null = FB_FALSE;

		out->n0 = 0;

		// In the execute method, the procedure scope must be accessed using the 'procedure' pointer.
		procedure->n1 = 0;

		// We don't touch n2 here, so it incremented counter will be kept after each execution.

		// The ResultSet scope must be accessed directly, i.e., they're member variables of the
		// 'this' pointer.
		++n4;
	}

	ISC_LONG n3;

	// FB_UDR_FETCH_PROCEDURE must be always after FB_UDR_EXECUTE_PROCEDURE.
	FB_UDR_FETCH_PROCEDURE
	{
		if (out->n0++ <= in->count)
		{
			out->n1 = ++procedure->n1;
			out->n2 = ++procedure->n2;
			out->n3 = ++n3;
			out->n4 = ++n4;
			return true;
		}

		return false;
	}

	ISC_LONG n4;
FB_UDR_END_PROCEDURE
